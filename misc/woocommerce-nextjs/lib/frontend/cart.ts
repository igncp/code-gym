import { ProductDetail } from "../models";
import { getFEModelClient } from "./client";

type CartItemId = string;

type CartItems = { [key: string]: number };

const STORAGE_KEY = "store:cart";

export type PopulatedCart = {
  [key: string]: { quantity: number; product: ProductDetail };
};

const saveCart = (cartItems: CartItems) => {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(cartItems));
};

export const clearCart = () => {
  localStorage.removeItem(STORAGE_KEY);
};

export const getCartItems = (): CartItems => {
  try {
    const str = localStorage.getItem(STORAGE_KEY);

    return str ? JSON.parse(str) || {} : {};
  } catch {
    return {};
  }
};

export const saveItemInCart = (id: CartItemId, quantity: number) => {
  const cartItems = getCartItems();

  cartItems[id] = (cartItems[id] || 0) + quantity;

  saveCart(cartItems);
};

export const deleteCartItem = () => {};

export const getCartItemsPopulated = async () => {
  const client = getFEModelClient();
  const cart = getCartItems();
  const keys = Object.keys(cart);
  const products = await client.getProducts(keys);

  return keys.reduce<PopulatedCart>((acc, key) => {
    const keyNum = Number(key);
    acc[key] = {
      product: products.find((p) => Number(p.id) === keyNum) as ProductDetail,
      quantity: cart[key],
    };

    return acc;
  }, {});
};
