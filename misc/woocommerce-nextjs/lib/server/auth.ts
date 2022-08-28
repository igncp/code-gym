import { getCookie, deleteCookie } from "cookies-next";
import {
  GetServerSidePropsContext,
  NextApiRequest,
  NextApiResponse,
} from "next/types";

type Opts =
  | GetServerSidePropsContext
  | { req: NextApiRequest; res: NextApiResponse };

export const getToken = (opts: Opts) => {
  const { req, res } = opts;

  return getCookie("token", { req, res }) as string;
};

export const deleteToken = (context: GetServerSidePropsContext) => {
  const { req, res } = context;
  deleteCookie("token", { req, res });
};

export const getIsLoggedIn = (opts: Opts) => {
  const token = getToken(opts);

  return !!token;
};
