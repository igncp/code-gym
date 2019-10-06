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

rimraf.sync(TEST_DIR);

const getRelativePath = absolutePath =>
  absolutePath.replace(`${REPO_DIR}/`, "");

const checkForConflicts = async context => {
  const repository = await Git.Repository.init(REPO_DIR, 0);
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

  const commit2 = await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Bar Message"
  );

  await repository.checkoutBranch("master", {});

  fs.writeFileSync(FILE_PATH, "baz");

  const commit3 = await repository.createCommitOnHead(
    [FILE_PATH].map(getRelativePath),
    signature,
    signature,
    "Baz Message"
  );

  const merge = await Git.Merge.commits(repository, commit3, commit2, null);

  console.log("hasConflicts", merge.hasConflicts() > 0);

  context.repository = repository;
};

const checkDiff = async context => {
  const { repository } = context;

  await repository.checkoutBranch("master");

  const commitFrom = await repository.getReferenceCommit("HEAD");

  await repository.checkoutBranch("develop");

  const commitTo = await repository.getReferenceCommit("HEAD");

  const content = await diff(repository, commitFrom, commitTo);

  console.log("content", content);
};

const main = async () => {
  const context = {};

  await checkForConflicts(context);
  await checkDiff(context);
};

main().catch(e => console.log(e));
