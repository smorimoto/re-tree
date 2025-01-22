type t = {
  dirs: ref(int),
  files: ref(int),
};

let count = {
  dirs: ref(0),
  files: ref(0),
};

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
          Printf.printf("%s└── %s\n", prefix, path);
          if (isDirectory) {
            tree(absolute, prefix ++ "    ", count);
          };
        } else {
          Printf.printf("%s├── %s\n", prefix, path);
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
  Printf.printf("\n%d directories, %d files\n", count.dirs^, count.files^);
};
