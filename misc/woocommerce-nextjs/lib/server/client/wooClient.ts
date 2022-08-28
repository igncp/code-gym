// @ts-expect-error
import WooCommerceRestApi from "@woocommerce/woocommerce-rest-api";
import { BASE_URL } from "../constants";

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
      version: "wc/v3",
    });
  }

  async getOrders() {
    return (await this.api.get("orders", {
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

  async postOrder(data: WCPostOrderOpts) {
    return (await this.api.post("orders", data)) as {
      data: WCPostOrderSuccess;
    };
  }
}

export const getWooClient = () => {
  return new WooClient();
};
