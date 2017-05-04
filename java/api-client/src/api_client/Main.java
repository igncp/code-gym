package api_client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import org.json.JSONObject;

public class Main {
	private static final String USER_AGENT = "FooBar";
	private static final String REQUEST_URL = "https://jsonplaceholder.typicode.com/posts/1";

	private static HttpGet getRequest() {
		HttpGet request = new HttpGet(REQUEST_URL);
		request.addHeader("User-Agent", USER_AGENT);

		return request;
	}

	private static HttpResponse getResponse(HttpGet request) throws IOException {
		HttpClient client = HttpClientBuilder.create().build();
		HttpResponse response = client.execute(request);

		return response;
	}

	public static void main(String[] args) throws IOException {
		HttpGet request = getRequest();
		HttpResponse response = getResponse(request);

		System.out.println("Response Code : " + response.getStatusLine().getStatusCode());

		HttpEntity entity = response.getEntity();
		InputStream is = entity.getContent();
		InputStreamReader ir = new InputStreamReader(is);
		BufferedReader rd = new BufferedReader(ir);

		StringBuffer result = new StringBuffer();
		String line = "";

		while ((line = rd.readLine()) != null) {
			result.append(line);
		}

		System.out.println(result);

		JSONObject obj = new JSONObject(result.toString());

		System.out.println("userId: " + obj.getInt("userId"));
		System.out.println("title: " + obj.getString("title"));
	}
}