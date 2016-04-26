package basic;

import java.sql.*;

public class MySQLOperations {
	private static Connection connection;

	public static void main(String[] args) {
		System.out.println("Starting operations:");
		connection = connect();

		printVersion();
		dropFooTableIfExists();
		createFooTable();
		insertRecordsIntoFooTable();
	}

	private static void executeStatement(String sql, String description) {
		System.out.println(description);

		try {
			Statement stmt = connection.createStatement();
			stmt.executeUpdate(sql);
		} catch (SQLException se) {
			se.printStackTrace();
		}
	}

	private static Connection connect() {
		String url = "jdbc:mysql://localhost:3306/javabase";
		String username = "java";
		String password = "pass";

		System.out.println("Connecting to database...");

		try {
			Connection connection = DriverManager.getConnection(url, username, password);
			return connection;
		} catch (SQLException e) {
			throw new IllegalStateException("Cannot connect the database!", e);
		}
	}
	
	private static void printVersion() {
		try {
			Statement stmt = connection.createStatement();
			ResultSet rs = stmt.executeQuery("SELECT VERSION()");
			if (rs.next()) {
                System.out.println("MySQL version: " + rs.getString(1));
            }
		} catch (SQLException se) {
			se.printStackTrace();
		}
	}

	private static void dropFooTableIfExists() {
		String sql = "DROP TABLE IF EXISTS foo";
		String description = "Removing table if it exists...";
		executeStatement(sql, description);
	}

	private static void createFooTable() {
		String sql = "CREATE TABLE foo (name VARCHAR(20))";
		String description = "Creating foo table...";
		executeStatement(sql, description);
	}

	public static void insertRecordsIntoFooTable() {
		String sql = "INSERT INTO foo VALUES ('bar'), ('baz')";
		String description = "Inserting records into foo table...";
		executeStatement(sql, description);
	}
}
