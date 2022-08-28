import type { NextApiRequest, NextApiResponse } from "next";

const signin = async (req: NextApiRequest, res: NextApiResponse) => {
  const { getServerModelClient } = await import("../../lib/server/client");

  const { body } = req;
  const client = getServerModelClient();
  const user = await client.login(JSON.parse(body));

  res.json(user);
};

export default signin;
