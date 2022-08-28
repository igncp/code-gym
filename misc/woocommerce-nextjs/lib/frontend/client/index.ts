import { PostOrderData, ProductDetail } from "../../models";

class ModelClient {
  constructor() {}

  async getProducts(ids: string[]): Promise<ProductDetail[]> {
    const response = await fetch("/api/products?ids=" + ids.join(","), {
      headers: {
        "Content-Type": "application/json",
      },
    });

    return await response.json();
  }

  async postOrder(body: PostOrderData) {
    const response = await fetch("/api/pay", {
      method: "POST",
      body: JSON.stringify(body),
      headers: {
        "Content-Type": "application/json",
      },
    });

    return await response.json();
  }

  async saveComment({ content, post }: { content: string; post: number }) {
    const result = await fetch("/api/comments", {
      method: "POST",
      body: JSON.stringify({
        content,
        post,
      }),
    });

    return await result.json();
  }

  async signup({
    email,
    password,
    username,
  }: {
    email: string;
    password: string;
    username: string;
  }) {
    const result = await fetch("/api/signup", {
      method: "POST",
      body: JSON.stringify({
        email,
        password,
        username,
      }),
    });
    const content = await result.json();

    return content;
  }
}

export const getFEModelClient = () => {
  return new ModelClient();
};
