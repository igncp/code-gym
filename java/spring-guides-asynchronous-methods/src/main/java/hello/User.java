package hello;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class User {

  private String name;
  private String location;
  private Integer public_repos;

  public String getName() {
    return name;
  }

  public String getLocation() {
    return location;
  }

  public Integer getPublic_repos() {
    return public_repos;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setLocation(String location) {
    this.location = location;
  }

  public void setPublic_repos(Integer public_repos) {
    this.public_repos = public_repos;
  }

  @Override
  public String toString() {
    return "User [name=" + name
        + (location != null ? ", location=<" + location + ">" : "")
        + ", public_repos=" + public_repos + "]";
  }

}