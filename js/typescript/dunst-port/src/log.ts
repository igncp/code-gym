import winston from "winston";

const logger = winston.createLogger({
  level: "info"
});

if (process.env.NODE_ENV !== "production") {
  logger.add(
    new winston.transports.Console({
      format: winston.format.simple()
    })
  );
}

const setLoggerLevel = (level: string) => {
  logger.level = level;
};

const info = (msg: string, ...args: any[]) => {
  logger.info(msg, ...args);
};

const debug = (msg: string, ...args: any[]) => {
  logger.debug(msg, ...args);
};

const logPlain = (msg: string, ...args: any[]) => {
  console.log(msg, ...args);
};

export { info, logPlain, debug, setLoggerLevel };
