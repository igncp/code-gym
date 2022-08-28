import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import Link from "next/link";
import { GetServerSidePropsContext } from "next/types";
import { PersonalInfo } from "../lib/models";

type Props = {
  me: PersonalInfo;
};

export default function Me({ me }: Props) {
  return (
    <div>
      <Header />
      <PageTitle text="Me" />
      <main>
        <div>Name: {me.name}</div>
      </main>
      <div>
        <Link href="/logout">Logout</Link>
      </div>
    </div>
  );
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const [{ getServerModelClient }, { getToken, deleteToken }] =
    await Promise.all([
      import("../lib/server/client"),
      import("../lib/server/auth"),
    ]);
  const token = getToken(context);
  const client = getServerModelClient({ token });
  const me = await client.getUserMe();

  if (me.code === "rest_not_logged_in") {
    deleteToken(context);

    return {
      props: {},
    };
  }

  return {
    props: {
      me,
    } as Props,
  };
};
