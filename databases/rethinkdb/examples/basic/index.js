// https://rethinkdb.com/docs/install-drivers/javascript/

const {always, partial, identity, partialRight, compose, unapply, ifElse, prop, equals} = require("ramda")
const {promisifyAll} = require("bluebird")
const r = promisifyAll(require("rethinkdb"))

const NOOP = () => null

const logWithMessage = message => compose(
  () => console.log(""),
  partial(console.log, [`${message}:`]),
  partialRight(JSON.stringify, [null, 4]),
  unapply(identity)
)

const CONNECTION_PROPS = {
  host: 'localhost',
  port: 28015,
}

const exitAfterCatch = conn => (error) => {
  console.log("error", error)
  return closeConnection(conn).then(() => {
    process.exit(1)
  })
}

const isReQLOpError = compose(equals("ReqlOpFailedError"), prop("name"))
const tableExistsError = isReQLOpError

const createDatabase = conn => r.dbCreate("test")
  .run(conn)
  .catch(ifElse(tableExistsError, NOOP, exitAfterCatch(conn)))
  .then(always(conn))

const dropTable = conn => r.db('test')
  .tableDrop("tv_shows").run(conn)
  .then(always(conn))
  .catch(exitAfterCatch(conn))

const dropTableIfExists = conn => r.db("test")
  .tableList().run(conn)
  .then(tables => tables.indexOf("tv_shows") > -1 ? dropTable(conn) : null)
  .then(always(conn))
  .catch(exitAfterCatch(conn))

const createTable = conn => r.db('test')
  .tableCreate('tv_shows').run(conn)
  .then(logWithMessage("Created table"))
  .then(always(conn))
  .catch(exitAfterCatch(conn))

const closeConnection = conn => conn.close()

const insertRecord = conn => r.table('tv_shows')
  .insert({name: "Star Trek TNG"}).run(conn)
  .then(logWithMessage("Inserted record"))
  .then(always(conn))
  .catch(exitAfterCatch(conn))

r.connect(CONNECTION_PROPS)
  .then(createDatabase)
  .then(dropTableIfExists)
  .then(createTable)
  .then(insertRecord)
  .then(closeConnection)
