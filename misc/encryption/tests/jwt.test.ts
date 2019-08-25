import test from "tape";
import jwt from "jsonwebtoken";

// https://github.com/auth0/node-jsonwebtoken#readme

const SIMPLE_SECRET = "shhhhh";
const SIMPLE_PAYLOAD = { foo: "bar" };

const createToken = () =>
  jwt.sign(SIMPLE_PAYLOAD, SIMPLE_SECRET, { noTimestamp: true });

test("tokens equality (with no timestamp)", { timeout: 5 * 1000 }, t => {
  t.plan(2);

  const tokenA = createToken();
  const tokenB = createToken();

  t.equal(tokenA, tokenB, "equal when created simultaneously");

  setTimeout(() => {
    const tokenC = createToken();

    t.equal(tokenA, tokenC, "equal when created at different times");
  }, 500);
});

test("tokens verification", t => {
  t.plan(3);

  const tokenA = createToken();

  jwt.verify(tokenA, SIMPLE_SECRET, (err, decoded) => {
    t.deepEqual(decoded, SIMPLE_PAYLOAD);
  });

  jwt.verify(tokenA, SIMPLE_SECRET + "WRONG", (err, decoded) => {
    t.deepEqual(
      err,
      {
        message: "invalid signature",
        name: "JsonWebTokenError"
      },
      "error is expected"
    );
    t.deepEqual(decoded, undefined, "there is no payload");
  });
});
