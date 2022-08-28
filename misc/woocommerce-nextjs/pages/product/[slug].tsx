import { GetServerSidePropsContext } from "next/types";
import Header from "../../components/common/header";
import PageTitle from "../../components/common/page-title";
import { saveItemInCart } from "../../lib/frontend/cart";
import { ProductDetail } from "../../lib/models";

type Props = {
  product: ProductDetail;
};

export default function Home({ product }: Props) {
  const addToCart = () => {
    saveItemInCart(product.id, 1);
  };
  return (
    <div>
      <Header />
      <PageTitle text={product.name} />
      {product.description && (
        <div dangerouslySetInnerHTML={{ __html: product.description }} />
      )}
      <div>
        Rating ({product.ratingCount}) - {product.ratingAverage}
      </div>
      <div>
        <button onClick={addToCart}>Add to cart</button>
      </div>
    </div>
  );
}

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const { getServerModelClient } = await import("../../lib/server/client");
  const client = getServerModelClient();

  const product = await client.getProduct({
    slug: context.query.slug as string,
  });

  const props: Props = {
    product,
  };

  return {
    props,
  };
};
