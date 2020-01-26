/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

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
