import type { NextApiRequest, NextApiResponse } from "next";

const signup = async (req: NextApiRequest, res: NextApiResponse) => {
  const { getServerModelClient } = await import("../../lib/server/client");
  const { body } = req;
  const client = getServerModelClient();
  const user = await client.createUser(JSON.parse(body));

  res.json(user);
};

export default signup;
