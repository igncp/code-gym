/**
 * Useful links:
 *
 * - https://www.nodegit.org/api/
 * - https://github.com/nodegit/nodegit/blob/master/examples
 * - https://github.com/thisconnect/nodegit-kit/tree/master/lib
 * - https://git-scm.com/book/en/v2/Git-Internals-Git-Objects
 * - libgit2.slack.com
 */
const path = require("path");
const fs = require("fs");

const rimraf = require("rimraf");

const Git = require("nodegit");

const diff = require("./diff");

const TEST_DIR = path.resolve(__dirname, "..", "tmp");
const REPO_DIR = path.resolve(TEST_DIR, "repo");
const FILE_PATH = path.resolve(REPO_DIR, "test.txt");

const getRelativePath = absolutePath =>
  absolutePath.replace(`${REPO_DIR}/`, "");

const resetRepo = async () => {
  rimraf.sync(TEST_DIR);

  await Git.Repository.init(REPO_DIR, 0);
};

const setRepoWithConflicts = async () => {
  await resetRepo();

  const repository = await Git.Repository.open(REPO_DIR);
  const signature = await Git.Signature.default(repository);

  fs.writeFileSync(FILE_PATH, "foo");

  const commit = await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Foo Message"
  );

  // doesn't automatically checkout to the branch
  await repository.createBranch("develop", commit, false);

  await repository.checkoutBranch("develop", {});

  fs.writeFileSync(FILE_PATH, "bar");

  await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Bar Message"
  );

  await repository.checkoutBranch("master", {});

  fs.writeFileSync(FILE_PATH, "baz");

  await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Baz Message"
  );
};

const setRepoWithoutConflicts = async () => {
  await resetRepo();

  const repository = await Git.Repository.open(REPO_DIR);
  const signature = await Git.Signature.default(repository);

  fs.writeFileSync(FILE_PATH, "foo");

  const commit = await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Foo Message"
  );

  // doesn't automatically checkout to the branch
  await repository.createBranch("develop", commit, false);

  await repository.checkoutBranch("develop", {});

  fs.writeFileSync(FILE_PATH, "bar");

  await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Bar Message"
  );
};

const checkIfBranchesHaveConflicts = async (branchA, branchB) => {
  if (branchA === branchB) {
    return 0;
  }

  const repository = await Git.Repository.open(REPO_DIR);

  // * https://www.nodegit.org/api/reference/
  const branch = await repository.getCurrentBranch();

  await repository.checkoutBranch(branchA, {});

  const commitFrom = await repository.getReferenceCommit("HEAD");

  await repository.checkoutBranch(branchB, {});

  const commitTo = await repository.getReferenceCommit("HEAD");

  const merge = await Git.Merge.commits(repository, commitFrom, commitTo, null);

  await repository.checkoutBranch(branch);

  return merge.hasConflicts() > 0;
};

const checkForConflicts = async () => {
  await setRepoWithConflicts();

  const resultWith = await checkIfBranchesHaveConflicts("master", "develop");

  await setRepoWithoutConflicts();

  const resultWithout = await checkIfBranchesHaveConflicts("master", "develop");

  console.log("doBranchesHaveConflicts (without)", resultWithout);
  console.log("doBranchesHaveConflicts (with)", resultWith);
};

const checkDiff = async () => {
  const repository = await Git.Repository.open(REPO_DIR);

  await repository.checkoutBranch("master");

  const commitFrom = await repository.getReferenceCommit("HEAD");

  await repository.checkoutBranch("develop");

  const commitTo = await repository.getReferenceCommit("HEAD");

  const content = await diff(repository, commitFrom, commitTo);

  console.log("content", content);
};

const stageFile = async () => {
  const repository = await Git.Repository.open(REPO_DIR);

  fs.writeFileSync(FILE_PATH, "123");

  const index = await repository.refreshIndex();

  await index.addByPath(getRelativePath(FILE_PATH));

  // this stages the commit
  await index.write();

  // to get the oid for creating the commit:
  // const oid = await index.writeTree();
};

const misc = async () => {
  const repository = await Git.Repository.open(REPO_DIR);

  const oid = await Git.Reference.nameToId(repository, "refs/heads/develop");

  console.log('oid', oid.tostrS());
};

const main = async () => {
  await checkForConflicts();
  await checkDiff();
  await stageFile();
  await misc();
};

main().catch(e => console.log(e));
