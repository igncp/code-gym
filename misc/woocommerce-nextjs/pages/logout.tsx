import { GetServerSidePropsContext } from "next/types";

export default function Logout() {
  return <div />;
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const { deleteToken } = await import("../lib/server/auth");

  deleteToken(context);

  return {
    redirect: {
      destination: "/",
      permanent: false,
    },
  };
};
