import WooCommerceRestApi from "@woocommerce/woocommerce-rest-api";

class WooClient {
  constructor() {
    this.api = new WooCommerceRestApi({
      url: "http://localhost",
      consumerKey: process.env.WOO_CONSUMER_KEY,
      consumerSecret: process.env.WOO_CONSUMER_SECRET,
      version: "wc/v3",
    });
  }

  async getOrders() {
    const response = await this.api.get("orders", {
      per_page: 20,
    });

    return response.data.map(({ id, total, currency_symbol }) => ({
      id,
      total,
      currencySymbol: currency_symbol,
    }));
  }

  async getPaymentGateways() {
    const response = await this.api.get("payment_gateways", {
      per_page: 20,
    });

    return response.data.map(({ id, title, enabled }) => ({
      id,
      title,
      enabled,
    }));
  }

  async getProduct(productId) {
    const response = await this.api.get("products/" + productId);

    return response.data;
  }

  async getProducts() {
    const response = await this.api.get("products", {
      per_page: 20,
    });

    return response.data.map(({ id, name }) => ({
      id,
      name,
    }));
  }

  async postOrder(data) {
    const response = await this.api.post("orders", data);

    return response.data;
  }
}

export const getWooClient = () => {
  return new WooClient();
};
