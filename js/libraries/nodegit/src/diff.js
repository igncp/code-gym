// Inspired in:
// https://github.com/thisconnect/nodegit-kit/blob/master/lib/diff.js

const getHunks = async hunk => {
  // * https://www.nodegit.org/api/diff_line/
  const lines = await hunk.lines();
  const diff = lines.map(line => {
    let change = " ";

    // * https://www.nodegit.org/api/diff_line/#newLineno
    // * https://www.nodegit.org/api/diff_line/#oldLineno
    if (line.newLineno() === -1) {
      change = "-";
    } else if (line.oldLineno() === -1) {
      change = "+";
    }

    // * https://www.nodegit.org/api/diff_line/#content
    return change + line.content().trim();
  });

  // header(): 'Something like `@@ -169,14 +167,12 @@ ...`'
  return [hunk.header().trim()].concat(diff).join("\n");
};

const getPatchStatus = patch => {
  // * https://www.nodegit.org/api/convenient_patch/
  if (patch.isUnmodified()) {
    return "unmodified";
  } else if (patch.isAdded()) {
    return "added";
  } else if (patch.isDeleted()) {
    return "deleted";
  } else if (patch.isModified()) {
    return "modified";
  } else if (patch.isRenamed()) {
    return "renamed";
  } else if (patch.isCopied()) {
    return "copied";
  } else if (patch.isIgnored()) {
    return "ignored";
  } else if (patch.isUntracked()) {
    return "untracked";
  } else if (patch.isTypeChange()) {
    return "typechange";
  }

  return "";
};

const getInfo = async patch => {
  const status = getPatchStatus(patch);
  // * https://www.nodegit.org/api/convenient_patch/#newFile
  // * https://www.nodegit.org/api/diff_file/
  const file = patch.newFile();
  const entry = {
    status: status,
    // * https://www.nodegit.org/api/diff_file/#path
    path: file.path(),
    // * https://www.nodegit.org/api/convenient_patch/#oldFile
    // * https://www.nodegit.org/api/diff_file/
    // * https://www.nodegit.org/api/diff_file/#id
    // * https://www.nodegit.org/api/oid/
    // * https://www.nodegit.org/api/oid/#tostrS
    sha1: patch
      .oldFile()
      .id()
      .tostrS(),
    sha: file.id().tostrS(),
    size: file.size()
  };

  if (entry.status !== "modified") {
    return entry;
  }

  const oldfile = patch.oldFile();

  if (entry.path !== oldfile.path()) {
    entry.oldpath = oldfile.path();
  }

  entry.oldsize = oldfile.size();

  // * https://www.nodegit.org/api/convenient_patch/#hunks
  const hunks = await patch.hunks();

  entry.hunks = await Promise.all(hunks.map(getHunks));

  return entry;
};

const getDiffPatches = async (current, tree) => {
  // * https://www.nodegit.org/api/tree/#diff
  // * https://www.nodegit.org/api/diff
  const diff = await current.diff(tree);

  // * https://www.nodegit.org/api/diff/#patches
  // * https://www.nodegit.org/api/convenient_patch/
  return diff.patches();
};

module.exports = async (repo, commitFrom, commitTo) => {
  // * https://www.nodegit.org/api/commit
  // * https://www.nodegit.org/api/commit/#getTree
  // * https://www.nodegit.org/api/tree
  const trees = await Promise.all([commitTo.getTree(), commitFrom.getTree()]);

  const patches = await getDiffPatches(trees[0], trees[1]);

  if (!patches.length) {
    return [];
  }

  const result = await Promise.all(patches.map(getInfo));

  return result;
};
