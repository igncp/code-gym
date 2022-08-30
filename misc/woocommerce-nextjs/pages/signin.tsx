import { useState } from "react";
import Header from "../components/common/header";
import cookie from "js-cookie";
import { useRouter } from "next/router";
import { GetServerSidePropsContext } from "next/types";
import PageTitle from "../components/common/page-title";

export default function SignIn() {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const router = useRouter();

  const onSave = async () => {
    const result = await fetch("/api/signin", {
      method: "POST",
      body: JSON.stringify({
        password,
        username,
      }),
    });
    const { token, code, message } = await result.json();

    if (code) {
      console.error(code, message);
      return;
    }

    cookie.set("token", token, {
      http: true,
    });

    router.replace("/me");
  };

  return (
    <div>
      <Header />
      <main>
        <PageTitle text="Sign in" />
        <div>
          <input
            type="text"
            placeholder="Username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
          />
        </div>
        <div>
          <input
            onChange={(e) => setPassword(e.target.value)}
            placeholder="Password"
            type="password"
            value={password}
          />
        </div>
        <div>
          <button onClick={onSave}>Submit</button>
        </div>
      </main>
    </div>
  );
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const { getToken } = await import("../lib/server/auth");
  const token = getToken(context);

  if (token) {
    return {
      redirect: {
        permanent: false,
        destination: "/me",
      },
    };
  }

  return {
    props: {},
  };
};
