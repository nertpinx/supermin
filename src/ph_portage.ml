(* supermin 5
 * Copyright (C) 2009-2014 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

open Unix
open Printf

open Utils
open Package_handler

let portage_detect () =
  Config.portageq <> "no" &&
  Config.qtbz2 <> "no" &&
    Os_release.get_id () = "gentoo" ||
     try (stat "/etc/gentoo-release").st_kind = S_REG with Unix_error _ -> false

let settings = ref no_settings

type portage_pkg_t = {
  category : string;
  name : string;
  version : string
}

let portage_pkg_to_str pkg =
  Printf.sprintf "%s/%s-%s" pkg.category pkg.name pkg.version

let parse_pv pf =
  let len = Str.search_forward (Str.regexp "-[0-9]") pf 0 in
  let len = len + 1 in
  String.sub pf 0 len, String.sub pf len (String.length pf - len)

let filter_dirs path x =
  let st = Unix.stat (path ^ x) in
  st.st_kind = Unix.S_DIR && String.get x 0 <> '-';;

let get_all_portage_pkgs =
  let db_path = "/var/db/pkg/" in
  let cats = Array.to_list (Sys.readdir db_path) in
  let cats = List.filter (filter_dirs db_path) cats in

  let names cat =
    let pkg (x, y) = { category = cat; name = x; version = y } in
    let catpath = db_path ^ cat ^ "/" in
    let lscat = Array.to_list (Sys.readdir catpath) in
    let lscat = List.filter (filter_dirs catpath) lscat in
    let lscat = List.map parse_pv lscat in
    List.map pkg lscat in

  let pkgs = List.map names cats in
  List.concat pkgs

let portage_pkg_of_pkg, pkg_of_portage_pkg = get_memo_functions ()

let portage_pkgs = Hashtbl.create 13
let portage_pkg_of_string str =
  if Hashtbl.length portage_pkgs == 0 then (
    let add_pkg pkg = Hashtbl.add portage_pkgs pkg.name pkg in
    List.iter add_pkg get_all_portage_pkgs
  );
  match Hashtbl.find_opt portage_pkgs str with
  | None -> None
  | Some pkg -> Some (pkg_of_portage_pkg pkg)

let portage_pkg_to_string pkg =
  let portage_pkg = portage_pkg_of_pkg pkg in
  sprintf "=%s/%s-%s" portage_pkg.category portage_pkg.name portage_pkg.version

let portage_pkg_name pkg = (portage_pkg_of_pkg pkg).name

let portage_get_package_database_mtime () =
  (lstat "/usr/portage/metadata/timestamp").st_mtime

let dpkg_get_all_requires pkgs =
  let dpkg_requires = Hashtbl.create 13 in
  (* Prepare dpkg_requires hashtbl with depends, pre-depends from all
     packages. Strip version information and discard alternative
     dependencies *)
  let cmd = sprintf "\
      %s --show --showformat='${Package} ${Depends} ${Pre-Depends}\n' | \
      sed -e 's/ *([^)]*) */ /g' \
          -e 's/ *, */ /g' \
          -e 's/ *| *[^ ]* */ /g'"
    Config.dpkg_query in
  let lines = run_command_get_lines cmd in
  List.iter (
    fun line ->
      match string_split " " line with
      | [] -> ()
      | pkgname :: [] -> ()
      | pkgname :: deps -> Hashtbl.add dpkg_requires pkgname deps
  ) lines;

  let get pkgs =
    let pkgnames = List.map dpkg_package_name (PackageSet.elements pkgs) in
    let deps = List.map (Hashtbl.find_all dpkg_requires) pkgnames in
    let deps = List.flatten (List.flatten deps) in
    let deps = filter_map dpkg_package_of_string deps in
    PackageSet.union pkgs (package_set_of_list deps)
  in
  (* The command above only gets one level of dependencies.  We need
   * to keep iterating until we reach a fixpoint.
   *)
  let rec loop pkgs =
    let pkgs' = get pkgs in
    if PackageSet.equal pkgs pkgs' then pkgs
    else loop pkgs'
  in
  loop pkgs

let dpkg_diversions = Hashtbl.create 13
let dpkg_get_all_files pkgs =
  if Hashtbl.length dpkg_diversions = 0 then (
    let cmd = sprintf "%s --list" Config.dpkg_divert in
    let lines = run_command_get_lines cmd in
    List.iter (
      fun line ->
        let items = string_split " " line in
        match items with
        | ["diversion"; "of"; path; "to"; real_path; "by"; pkg] ->
          Hashtbl.add dpkg_diversions path real_path
        | _ -> ()
    ) lines
  );
  let cmd =
    sprintf "%s --listfiles %s | grep '^/' | grep -v '^/.$' | sort -u"
      Config.dpkg_query
      (quoted_list (List.map dpkg_package_name_arch
		      (PackageSet.elements pkgs))) in
  let lines = run_command_get_lines cmd in
  List.map (
    fun path ->
      let config =
	try string_prefix "/etc/" path && (lstat path).st_kind = S_REG
	with Unix_error _ -> false in
      let source_path =
        try Hashtbl.find dpkg_diversions path
        with Not_found -> path in
      { ft_path = path; ft_source_path = source_path; ft_config = config }
  ) lines

let dpkg_download_all_packages pkgs dir =
  let tdir = !settings.tmpdir // string_random8 () in
  mkdir tdir 0o755;

  let dpkgs = List.map dpkg_package_name (PackageSet.elements pkgs) in

  let cmd =
    sprintf "cd %s && %s %s download %s"
      (quote tdir)
      Config.apt_get
      (if !settings.debug >= 1 then "" else " --quiet --quiet")
      (quoted_list dpkgs) in
  run_command cmd;

  (* Unpack each downloaded package. *)
  let cmd =
    sprintf "
umask 0000
for f in %s/*.deb; do
  %s --fsys-tarfile \"$f\" | (cd %s && tar xf -)
done"
      (quote tdir) Config.dpkg_deb (quote dir) in
  run_command cmd

let () =
  let ph = {
    ph_detect = portage_detect;
    ph_init = (fun () -> ());
    ph_fini = (fun () -> ());
    ph_package_of_string = portage_pkg_of_string;
    ph_package_to_string = portage_pkg_to_string;
    ph_package_name = portage_pkg_name;
    ph_get_package_database_mtime = portage_get_package_database_mtime;
    ph_get_requires = PHGetAllRequires dpkg_get_all_requires;
    ph_get_files = PHGetAllFiles dpkg_get_all_files;
    ph_download_package = PHDownloadAllPackages dpkg_download_all_packages;
  } in
  register_package_handler "gentoo" "portage" ph
