type Props = {
  text: string;
};

export default function PageTitle({ text }: Props) {
  return <h2>{text}</h2>;
}
