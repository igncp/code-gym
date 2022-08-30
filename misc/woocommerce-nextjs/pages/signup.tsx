import { useState } from "react";
import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import { getFEModelClient } from "../lib/frontend/client";

export default function Home() {
  const [email, setEmail] = useState("");
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");

  const onSave = async () => {
    const client = getFEModelClient();

    await client.signup({
      email,
      password,
      username,
    });

    setEmail("");
    setUsername("");
    setPassword("");
  };

  return (
    <div>
      <Header />
      <main>
        <PageTitle text="Sign up" />
        <div>
          <input
            type="text"
            value={email}
            placeholder="Email"
            onChange={(e) => setEmail(e.target.value)}
          />
        </div>
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
            type="password"
            placeholder="Password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
          />
        </div>
        <div>
          <button onClick={onSave}>Save</button>
        </div>
      </main>
    </div>
  );
}

export const getServerSideProps = async () => {
  return {
    props: {},
  };
};
