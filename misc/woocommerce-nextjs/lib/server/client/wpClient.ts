import WPAPI from "wpapi";
import { BASE_URL } from "../constants";

const REST_BASE_URL = BASE_URL + "/wp-json";

type Rendered = {
  rendered: string;
};

type WPCommentSummary = {
  content: Rendered;
  id: string;
};

type WPPageDetail = {
  content: Rendered;
  id: string;
  title: Rendered;
};

type WPPersonalInfo = {
  code: string;
  email: string;
  id: number;
  name: string;
};

type WPPostSummary = {
  id: string;
  title: Rendered;
};

class WPClient {
  private api: any;
  private token: string | undefined;

  public constructor(token?: string) {
    this.token = token;
    this.api = new WPAPI({
      endpoint: REST_BASE_URL + "/",
    });
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
    const { token: adminToken } = await this.login({
      password: process.env.WP_APP_PASS as string,
      username: "admin",
    });

    return await fetch(REST_BASE_URL + "/wp/v2/users/", {
      method: "POST",
      body: JSON.stringify(newUser),
      headers: {
        "Content-Type": "application/json",
        Authorization: "Bearer " + adminToken,
      },
    });
  }

  async getComments(postId: string) {
    return (await this.api.comments().post(postId).get()) as WPCommentSummary[];
  }

  async getPage(id: string) {
    const pages = (await this.api.pages().slug(id).get()) as WPPageDetail[];
    const page = pages?.[0];

    return page;
  }

  async getPost(id: string) {
    const { title, content } = await this.api.posts().id(id).get();

    return { title, content };
  }

  async getPosts() {
    return (await this.api.posts().get()) as WPPostSummary[];
  }

  async getUser({ id }: { id: string }) {
    const result = await fetch(
      REST_BASE_URL + "/wp/v2/users/" + id + "?context=edit",
      {
        headers: {
          "Content-Type": "application/json",
          Authorization: "Bearer " + this.token,
        },
      }
    );

    const {
      name,
      code = "",
      id: realId,
      email,
    } = (await result.json()) as WPPersonalInfo;

    return { name, code, id: realId, email };
  }

  async login(userCreds: { username: string; password: string }) {
    // This requires installing the JWT token plugin plus updating wp-config.php with the JWT key
    const result = await fetch(REST_BASE_URL + "/jwt-auth/v1/token/", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(userCreds),
    });

    return await result.json();
  }

  async postComment(data: any) {
    const result = await fetch(REST_BASE_URL + "/wp/v2/comments", {
      body: data,
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: "Bearer " + this.token,
      },
    });

    return (await result.json()) as WPPersonalInfo;
  }
}

export const getWPClient = (token?: string) => {
  return new WPClient(token);
};
