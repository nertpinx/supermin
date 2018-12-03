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

let db_path = "/var/db/pkg"

let portage_detect () =
  Config.portageq <> "no" &&
  Config.qtbz2 <> "no" &&
    Os_release.get_id () = "gentoo" ||
     try (stat "/etc/gentoo-release").st_kind = S_REG with Unix_error _ -> false

let settings = ref no_settings

let portage_init s =
  settings := s;

type portage_pkg_t = {
  category : string;
  name : string;
  version : string
}

let portage_pkg_to_str pkg =
  sprintf "%s/%s-%s" pkg.category pkg.name pkg.version

(* Parses ">=category/name-version[rest]" as well as just "name-version" *)
let parse_pv pf =
  let get_two exp x = Str.bounded_split (Str.regexp exp) x 2 in
  let tmp = get_two "/" pf in
  let category =
    if List.length tmp = 2 then
      let ctmp = List.hd tmp in
      let len = Str.search_forward (Str.regexp "[a-zA-Z]") ctmp 0 in
      String.sub ctmp len (String.length ctmp - len)
    else "" in
  let tmp = if List.length tmp = 2 then List.hd @@ List.tl @@ tmp else List.hd tmp in
  let len = Str.search_forward (Str.regexp "-[0-9]") tmp 0 in
  let name = String.sub tmp 0 len in
  let len = len + 1 in
  let tmp = String.sub tmp len (String.length tmp - len) in
  let tmp = get_two "[:[]" tmp in
  let version = List.hd tmp in
  category, name, version

let filter_dirs path x =
  let st = Unix.stat (path // x) in
  st.st_kind = Unix.S_DIR && String.get x 0 <> '-';;

let get_all_portage_pkgs =
  let cats = Array.to_list (Sys.readdir db_path) in
  let cats = List.filter (filter_dirs db_path) cats in

  let names cat =
    let pkg (_, x, y) =
      { category = cat; name = x; version = y } in
    let catpath = db_path // cat in
    let lscat = Array.to_list (Sys.readdir catpath) in
    let lscat = List.filter (filter_dirs catpath) lscat in
    let lscat = List.map parse_pv lscat in
    List.map pkg lscat in

  let pkgs = List.map names cats in
  List.concat pkgs

let get_all_portageq_pkgs =
  let cats = Array.to_list (Sys.readdir db_path) in
  let cats = List.filter (filter_dirs db_path) cats in

  let names cat =
    let pkg (_, x, y) =
      { category = cat; name = x; version = y } in
    let catpath = db_path // cat in
    let lscat = Array.to_list (Sys.readdir catpath) in
    let lscat = List.filter (filter_dirs catpath) lscat in
    let lscat = List.map parse_pv lscat in
    List.map pkg lscat in

  let pkgs = List.map names cats in
  List.concat pkgs

let portage_pkg_of_pkg, pkg_of_portage_pkg = get_memo_functions ()

let portage_pkgs_by_name = Hashtbl.create 13
let portage_pkgs_by_catname = Hashtbl.create 13
let portage_pkg_of_string str =
  print_endline (__LOC__ ^ ": " ^ str);
  if Hashtbl.length portage_pkgs_by_name == 0 then (
    let add_pkg pkg =
      Hashtbl.add portage_pkgs_by_name pkg.name pkg;
      Hashtbl.add portage_pkgs_by_catname (pkg.category // pkg.name) pkg in
    List.iter add_pkg get_all_portageq_pkgs
  );
  (* Prefer searching by full name, but allow just name as well *)
  match Hashtbl.find_opt portage_pkgs_by_catname str with
  | Some pkg -> Some (pkg_of_portage_pkg pkg)
  | None -> (match Hashtbl.find_opt portage_pkgs_by_name str with
      | Some pkg -> Some (pkg_of_portage_pkg pkg)
      | None -> None)

let portage_pkg_to_string pkg = portage_pkg_to_str @@ portage_pkg_of_pkg @@ pkg

let portage_pkg_name pkg =
  let pkg = portage_pkg_of_pkg pkg in
  sprintf "%s/%s" pkg.category pkg.name

let portage_get_package_database_mtime () =
  (lstat "/usr/portage/metadata/timestamp").st_mtime

let portage_pkg_path pkg =
  db_path // pkg.category // (pkg.name ^ "-" ^ pkg.version)

let find_usable atom =
  let cmd =
    sprintf "%s best_visible / installed %s" Config.portageq (quote atom) in
  let out = run_command_get_lines cmd in
  match out with
  |[] -> None
  |x :: [] -> Some x
  |_ -> error "Unknown output of 'best_visible'"

let pkg_of_string str =
  let make_pkg (x, y, z) = { category = x; name = y; version = z } in
  pkg_of_portage_pkg @@ make_pkg @@ parse_pv @@ str

let portage_get_requires pkg =
  let ret = ref (PackageSet.add pkg PackageSet.empty) in
  let deps = (portage_pkg_path @@ portage_pkg_of_pkg @@ pkg) // "RDEPEND" in
  let deps = String.split_on_char ' ' @@ input_line @@ open_in @@ deps in
  let add_pkg atom =
    (match (find_usable atom) with
    |None -> raise (Failure "Inconsistent system, no installed package satisfies")
    |Some pkg ->
      ret := PackageSet.add (pkg_of_string pkg) !ret) in
  List.iter add_pkg deps;
  PackageSet.iter (fun x -> print_endline (portage_pkg_to_string x)) !ret;
  !ret

let portage_get_files pkg =
  let file_to_file_t filename =
    let cmd = sprintf "%s is_protected / %s" Config.portageq (quote filename) in
    let config = Sys.command cmd = 0 in
    { ft_path = filename; ft_source_path = filename; ft_config = config } in
  let atom = portage_pkg_to_str @@ portage_pkg_of_pkg pkg in
  let cmd = sprintf "%s contents / %s" Config.portageq (quote atom) in
  let files = run_command_get_lines cmd in
  List.map file_to_file_t files

let portage_download_all_packages pkgs dir =
  let tdir = !settings.tmpdir // string_random8 () in
  mkdir tdir 0o755;

  let cmd =
    sprintf "
cd %s
touch asdf.txt"
      (quote tdir) in
  run_command cmd

let () =
  let ph = {
    ph_detect = portage_detect;
    ph_init = portage_init;
    ph_fini = (fun () -> ());
    ph_package_of_string = portage_pkg_of_string;
    ph_package_to_string = portage_pkg_to_string;
    ph_package_name = portage_pkg_name;
    ph_get_package_database_mtime = portage_get_package_database_mtime;
    ph_get_requires = PHGetRequires portage_get_requires;
    ph_get_files = PHGetFiles portage_get_files;
    ph_download_package = PHDownloadAllPackages portage_download_all_packages;
  } in
  register_package_handler "gentoo" "portage" ph
