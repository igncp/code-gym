export type CommentSummary = {
  content: string;
  id: string;
};

export type OrderSummary = {
  currencySymbol: string;
  id: string;
  total: number;
};

export type PageDetail = {
  content: string;
  id: string;
  title: string;
};

export type PaymentGateway = {
  enabled: boolean;
  id: string;
  title: string;
};

export type PersonalInfo = {
  code: string;
  name: string;
};

export type PostDetail = {
  id: string;
  title: string;
  content: string;
};

export type PostOrderData = {
  items: Array<{ productId: number; quantity: number }>;
};

export type PostSummary = {
  id: string;
  title: string;
};

export type ProductDetail = {
  description: string;
  id: string;
  name: string;
  ratingAverage: string;
  ratingCount: number;
};

export type ProductSummary = {
  id: string;
  name: string;
  priceHTML: string;
  slug: string;
};
