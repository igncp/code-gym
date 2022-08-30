import { GetServerSidePropsContext } from "next/types";
import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import { PageDetail } from "../lib/models";

type Props = {
  page: PageDetail;
};

export default function Home({ page }: Props) {
  return (
    <div>
      <Header />
      <PageTitle text={page.title} />
      <div dangerouslySetInnerHTML={{ __html: page.content }} />
    </div>
  );
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const { getServerModelClient } = await import("../lib/server/client");
  const client = getServerModelClient();
  try {
    const page = await client.getPage(context.query.id as string);
    const props = {
      page,
    };

    return {
      props,
    };
  } catch {
    return {
      notFound: true,
    };
  }
};
