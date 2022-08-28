import Link from "next/link";
import { GetServerSidePropsContext } from "next/types";
import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import { ProductSummary } from "../lib/models";

type Props = {
  isLoggedIn: boolean;
  products: ProductSummary[];
};

export default function Home({ products, isLoggedIn }: Props) {
  return (
    <div>
      <Header />
      <PageTitle text="Shop" />
      <main>
        {products.map((product) => {
          return (
            <div key={product.id}>
              <Link href={"/product/" + product.slug} passHref>
                <a>
                  {product.name} -{" "}
                  <span
                    dangerouslySetInnerHTML={{ __html: product.priceHTML }}
                  />
                </a>
              </Link>
            </div>
          );
        })}
        <hr />
        <div>
          <Link href={"/checkout"}>Checkout</Link>
        </div>
        <div>
          <Link href={"/blog"}>Blog</Link>
        </div>
        <div>
          <Link href={"/cart"}>Cart</Link>
        </div>
        {!isLoggedIn ? (
          <>
            <div>
              <Link href={"/signup"}>Sign up</Link>
            </div>
            <div>
              <Link href={"/signin"}>Sign in</Link>
            </div>
          </>
        ) : (
          <>
            <div>
              <Link href={"/orders"}>Orders</Link>
            </div>
            <div>
              <Link href={"/logout"}>Logout</Link>
            </div>
          </>
        )}
      </main>
    </div>
  );
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const [{ getServerModelClient }, { getIsLoggedIn }] = await Promise.all([
    import("../lib/server/client"),
    import("../lib/server/auth"),
  ]);
  const serverModelClient = getServerModelClient();
  const isLoggedIn = getIsLoggedIn(context);

  const products = await serverModelClient.getProducts();
  const props: Props = {
    isLoggedIn,
    products,
  };

  return {
    props,
  };
};
