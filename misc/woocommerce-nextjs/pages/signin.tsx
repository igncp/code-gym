import { useState } from "react";
import Header from "../components/common/header";
import cookie from "js-cookie";
import { useRouter } from "next/router";
import { GetServerSidePropsContext } from "next/types";

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
    const { token } = await result.json();

    cookie.set("token", token, {
      http: true,
    });

    router.replace("/me");
  };

  return (
    <div>
      <Header />
      <main>
        <h2>Signup</h2>
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
            type="text"
            placeholder="Password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
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
