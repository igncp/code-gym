import type { NextApiRequest, NextApiResponse } from "next";

const comments = async (req: NextApiRequest, res: NextApiResponse) => {
  const [{ getServerModelClient }, { getToken }] = await Promise.all([
    import("../../lib/server/client"),
    import("../../lib/server/auth"),
  ]);
  const token = getToken({ req, res });
  const client = getServerModelClient({ token });

  const { body } = req;
  const result = client.saveComment(body);

  res.json(result);
};

export default comments;
