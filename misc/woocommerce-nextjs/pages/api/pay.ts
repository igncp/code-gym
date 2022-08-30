import type { NextApiRequest, NextApiResponse } from "next";
import { PostOrderData } from "../../lib/models";

const pay = async (req: NextApiRequest, res: NextApiResponse) => {
  const { getServerModelClient } = await import("../../lib/server/client");
  const client = getServerModelClient();
  const input = req.body as PostOrderData;

  const data = {
    payment_method: "bacs",
    payment_method_title: "Direct Bank Transfer",
    set_paid: true,
    billing: {
      first_name: "John",
      last_name: "Doe",
      address_1: "969 Market",
      address_2: "",
      city: "San Francisco",
      state: "CA",
      postcode: "94103",
      country: "US",
      email: "foo@bar.com",
      phone: "(555) 555-5555",
    },
    shipping: {
      first_name: "John",
      last_name: "Doe",
      address_1: "969 Market",
      address_2: "",
      city: "San Francisco",
      state: "CA",
      postcode: "94103",
      country: "US",
    },
    line_items: input.items.map((item) => {
      return {
        ...item,
        product_id: item.productId,
      };
    }),
    shipping_lines: [
      {
        method_id: "flat_rate",
        method_title: "Flat Rate",
        total: "10.00",
      },
    ],
  };

  const response = await client.postOrder(data);

  res.json(response);
};

export default pay;
