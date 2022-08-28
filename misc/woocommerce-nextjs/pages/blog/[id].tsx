import { useRouter } from "next/router";
import { GetServerSidePropsContext } from "next/types";
import { useState } from "react";
import Header from "../../components/common/header";
import PageTitle from "../../components/common/page-title";
import { getFEModelClient } from "../../lib/frontend/client";
import { CommentSummary, PostDetail } from "../../lib/models";

type Props = {
  comments: CommentSummary[];
  isLoggedIn: boolean;
  post: PostDetail;
};

type Query = {
  id: string;
};

export default function BlogId({ post, isLoggedIn, comments }: Props) {
  const { query } = useRouter();
  const [content, setContent] = useState("");

  const saveComment = async () => {
    const client = getFEModelClient();

    await client.saveComment({
      content,
      post: Number(query.id),
    });

    setContent("");
  };

  return (
    <div>
      <Header />
      <main>
        <PageTitle text={post.title} />
        <div dangerouslySetInnerHTML={{ __html: post.content }} />
        {isLoggedIn && (
          <div>
            <div>
              <textarea
                onChange={(e) => setContent(e.target.value)}
                placeholder="Message"
                value={content}
              />
            </div>
            <div>
              <button onClick={saveComment}>Save comment</button>
            </div>
          </div>
        )}
        {comments.map((comment) => {
          return (
            <div
              key={comment.id}
              dangerouslySetInnerHTML={{ __html: comment.content }}
              style={{ border: "1px solid #000" }}
            />
          );
        })}
      </main>
    </div>
  );
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext<Query>
) => {
  const [{ getServerModelClient }, { getIsLoggedIn }] = await Promise.all([
    import("../../lib/server/client"),
    import("../../lib/server/auth"),
  ]);
  const client = getServerModelClient();
  const isLoggedIn = getIsLoggedIn(context);
  const [post, comments] = await Promise.all([
    client.getPost(context.query.id as string),
    client.getComments(context.query.id as string),
  ]);

  const props: Props = {
    comments,
    isLoggedIn,
    post,
  };

  return {
    props,
  };
};
