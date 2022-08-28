import Link from "next/link";
import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import { PostSummary } from "../lib/models";

type Props = {
  posts: PostSummary[];
};

export default function Blog({ posts }: Props) {
  return (
    <div>
      <Header />
      <PageTitle text="Blog" />
      {posts.map((post) => {
        return (
          <div key={post.id}>
            <Link href={"/blog/" + post.id}>{post.title}</Link>
          </div>
        );
      })}
    </div>
  );
}

export const getServerSideProps = async () => {
  const { getServerModelClient } = await import("../lib/server/client");
  const client = getServerModelClient();
  const posts = await client.getPosts();
  const props: Props = {
    posts,
  };

  return {
    props,
  };
};
