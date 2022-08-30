import {
  CommentSummary,
  OrderSummary,
  PageDetail,
  PaymentGateway,
  PersonalInfo,
  PostDetail,
  PostSummary,
  ProductDetail,
  ProductSummary,
} from "../../models";
import { getWooClient } from "./wooClient";
import { getWPClient } from "./wpClient";

type ConstructorOpts = Partial<{ token: string }>;

class ModelClient {
  private wooClient: ReturnType<typeof getWooClient>;
  private wpClient: ReturnType<typeof getWPClient>;

  constructor(opts: ConstructorOpts = {}) {
    this.wooClient = getWooClient();
    this.wpClient = getWPClient(opts.token);
  }

  async createUser({
    email,
    password,
    username,
  }: {
    email: string;
    password: string;
    username: string;
  }) {
    const newUser = { username, password, email };

    return await this.wpClient.createUser(newUser);
  }

  async getComments(postId: string): Promise<CommentSummary[]> {
    const comments = await this.wpClient.getComments(postId);

    return comments.map(({ id, content }) => ({
      content: content.rendered,
      id,
    }));
  }

  async getCustomer({ email }: { email: string }): Promise<any> {
    // const orders = await this.wooClient.getOrders();
    // console.log("index.ts: orders", orders.data.length);
    const customers = await this.wooClient.getCustomers({ email });
    console.log("index.ts: customers", customers);

    if (!customers.length) return null;

    return customers[0];
  }

  async getPage(id: string): Promise<PageDetail> {
    const page = await this.wpClient.getPage(id);

    if (!page) throw new Error("Missing page: " + id);

    return {
      id,
      title: page.title?.rendered || "",
      content: page.content?.rendered || "",
    };
  }

  async getOrders(): Promise<OrderSummary[]> {
    const response = await this.wooClient.getOrders();

    return response.data.map(({ id, total, currency_symbol }) => ({
      id,
      total,
      currencySymbol: currency_symbol,
    }));
  }

  async getPaymentGateways(): Promise<PaymentGateway[]> {
    const response = await this.wooClient.getPaymentGateways();

    return response.data.map(({ id, title, enabled }) => ({
      id,
      title,
      enabled,
    }));
  }

  async getPost(id: string): Promise<PostDetail> {
    const { title, content } = await this.wpClient.getPost(id);

    return {
      id,
      title: title?.rendered || "",
      content: content?.rendered || "",
    };
  }

  async getPosts(): Promise<PostSummary[]> {
    const posts = await this.wpClient.getPosts();

    return posts.map(({ id, title }) => ({
      id,
      title: title?.rendered || "",
    }));
  }

  async getProduct(
    opts: Parameters<ModelClient["wooClient"]["getProducts"]>[0]
  ): Promise<ProductDetail> {
    const products = await this.wooClient.getProducts(opts);
    const product = Array.isArray(products?.data)
      ? products.data?.[0] || {}
      : products?.data;

    const {
      average_rating: ratingAverage,
      description = "",
      id,
      name = "",
      rating_count: ratingCount,
    } = product;

    return {
      description,
      id,
      name,
      ratingAverage,
      ratingCount,
    };
  }

  async getProducts(): Promise<ProductSummary[]> {
    const response = await this.wooClient.getProducts();

    return response.data.map(({ id, name, slug, price_html: priceHTML }) => ({
      id,
      name,
      priceHTML,
      slug,
    }));
  }

  async getUserMe(): Promise<PersonalInfo> {
    const { name, code, id, email } = await this.wpClient.getUser({ id: "me" });

    return { name, code, email, id };
  }

  async login(userCreds: { username: string; password: string }) {
    return await this.wpClient.login(userCreds);
  }

  async postOrder(data: Parameters<ModelClient["wooClient"]["postOrder"]>[0]) {
    const response = await this.wooClient.postOrder(data);

    return response.data;
  }

  async saveComment(data: any) {
    const response = await this.wpClient.postComment(data);

    return response;
  }
}

export const getServerModelClient = (opts?: ConstructorOpts) => {
  return new ModelClient(opts);
};
