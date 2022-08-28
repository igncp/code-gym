import type { NextApiRequest, NextApiResponse } from "next";

const signin = async (req: NextApiRequest, res: NextApiResponse) => {
  const { getServerModelClient } = await import("../../lib/server/client");

  const idsStr = req.query.ids as string;

  if (!idsStr) {
    return res.json([]);
  }

  const ids = idsStr.split(",");
  const client = getServerModelClient();

  const products = await Promise.all(
    ids.map((id) => {
      return client.getProduct({ id });
    })
  );

  res.json(products);
};

export default signin;
