type t = {
  dirs: ref(int),
  files: ref(int),
};

let count = {dirs: ref(0), files: ref(0)};

let rec tree = (directory, prefix, count) => {
  let paths = Sys.readdir(directory);
  Array.iteri(
    (index, path) =>
      if (!Char.equal(path.[0], '.')) {
        let absolute = Filename.concat(directory, path);
        let isDirectory = Sys.is_directory(absolute);
        if (isDirectory) {
          count.dirs := count.dirs^ + 1;
        } else {
          count.files := count.files^ + 1;
        };
        if (index == Array.length(paths) - 1) {
          print_endline(prefix ++ "└── " ++ path);
          if (isDirectory) {
            tree(absolute, prefix ++ "    ", count);
          };
        } else {
          print_endline(prefix ++ "├── " ++ path);
          if (isDirectory) {
            tree(absolute, prefix ++ "│   ", count);
          };
        };
      },
    paths,
  );
};

let () = {
  tree(Sys.getcwd(), "", count);
  print_endline(
    "\n"
    ++ string_of_int(count.dirs^)
    ++ " directories, "
    ++ string_of_int(count.files^)
    ++ " files",
  );
};
