package hello;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

@Service
public class DAO {

  @Autowired
  JdbcTemplate jdbcTemplate;

  private void initTable() {
    jdbcTemplate.execute("DROP TABLE customers IF EXISTS");
    jdbcTemplate.execute(
        "CREATE TABLE customers(id SERIAL, first_name VARCHAR(255), last_name VARCHAR(255))");
  }

  private void insertRecords() {
    List<Object[]> splitUpNames = Arrays
        .asList("John Woo", "Jeff Dean", "Josh Bloch", "Josh Long").stream()
        .map(name -> name.split(" ")).collect(Collectors.toList());

    jdbcTemplate.batchUpdate(
        "INSERT INTO customers(first_name, last_name) VALUES (?,?)",
        splitUpNames);
  }

  public Customer queryCustomer(String firstName) {
    String sql = "SELECT id, first_name, last_name FROM customers WHERE first_name = ?";
    Customer customer = jdbcTemplate.queryForObject(sql,
        new Object[] { firstName },
        (rs, rowNum) -> new Customer(rs.getLong("id"),
            rs.getString("first_name"), rs.getString("last_name")));

    return customer;
  }

  public void setup() {
    initTable();
    insertRecords();

  }
}
