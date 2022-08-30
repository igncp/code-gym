// @ts-expect-error
// https://github.com/woocommerce/woocommerce-rest-api-js-lib
import WooCommerceRestApi from "@woocommerce/woocommerce-rest-api";
import { BASE_URL } from "../constants";

type WCCustomerSummary = {
  id: number;
};

type WCOrderSummary = {
  currency_symbol: string;
  id: string;
  total: number;
};

type WCPaymentGateway = {
  id: string;
  title: string;
  enabled: boolean;
};

type WCProductSummary = {
  average_rating: string;
  description: string;
  id: string;
  name: string;
  price: string;
  price_html: string;
  rating_count: number;
  slug: string;
};

type WCPostOrderOpts = {
  line_items: Array<{
    product_id: number;
    quantity: number;
    product_variation?: number;
  }>;
};

type WCPostOrderSuccess = {
  currency: string;
  currency_symbol: string;
};

type GetProductsOpts = Partial<{ slug: string; id: string; per_page: number }>;

class WooClient {
  private api: any;

  constructor() {
    this.api = new WooCommerceRestApi({
      url: BASE_URL,
      consumerKey: process.env.WOO_CONSUMER_KEY,
      consumerSecret: process.env.WOO_CONSUMER_SECRET,
      wpAPIPrefix: "wp-json",
      version: "wc/v3",
    });
  }

  async getCustomers(query: { email: string }) {
    console.log("wooClient.ts: query", query);
    const result = await this.api.get("customers", {
      per_page: 20,
    });
    console.log("wooClient.ts: result", result.data);

    return result.data;
  }

  async getOrders() {
    return (await this.api.get("/orders", {
      per_page: 20,
    })) as { data: WCOrderSummary[] };
  }

  async getPaymentGateways() {
    return (await this.api.get("payment_gateways", {
      per_page: 20,
    })) as { data: WCPaymentGateway[] };
  }

  async getProducts({ id, ...rest }: GetProductsOpts = {}) {
    return (await this.api.get(
      "products" + (id ? "/" + id : ""),
      Object.assign(
        {
          per_page: 20,
        },
        rest
      )
    )) as { data: WCProductSummary[] };
  }

  // https://woocommerce.github.io/woocommerce-rest-api-docs/#create-an-order
  async postOrder(data: WCPostOrderOpts) {
    return (await this.api.post("orders", data)) as {
      data: WCPostOrderSuccess;
    };
  }
}

export const getWooClient = () => {
  return new WooClient();
};
