package io.deepstream;

import com.google.gson.Gson;
import javafx.util.Pair;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

public class UtilObjectDifferTest {

    UtilObjectDiffer comparer;
    Gson gson;

    @Before
    public void setUp() throws URISyntaxException {
        comparer = new UtilObjectDiffer();
        gson = new Gson();
    }

    @After
    public void tearDown() {

    }

    @org.junit.Test
    public void sameObjectSendsNoUpdateOrPatch() {
        Person p = new Person(20, "Alex", new ArrayList());

        Pair pair = comparer.getDiff(p, p);

        Assert.assertEquals("", pair.getKey());
        Assert.assertEquals(null, pair.getValue());
    }

    @org.junit.Test
    public void simpleObjectSendsPatch() {
        Person p = new Person(20, "Alex", new ArrayList());
        Person q = new Person(20, "Craig", new ArrayList());

        Pair pair = comparer.getDiff(p, q);

        Assert.assertEquals("name", pair.getKey());
        Assert.assertEquals("Craig", pair.getValue());
    }

    @org.junit.Test
    public void simpleObjectSendsPatchForWholeList() {

        ArrayList listA = new ArrayList();
        listA.add("beer");
        listA.add("coffee");
        ArrayList listB = new ArrayList();
        listB.add("wine");
        listB.add("coffee");

        Person p = new Person(20, "Alex", listA);
        Person q = new Person(20, "Alex", listB);

        Pair pair = comparer.getDiff(p, q);

        Assert.assertEquals("likes", pair.getKey());
        Assert.assertEquals( "[\"wine\",\"coffee\"]", getJson(pair.getValue()) );
    }

    @org.junit.Test
    public void simpleObjectSendsUpdate() {

        ArrayList listA = new ArrayList();
        listA.add("beer");
        listA.add("coffee");
        ArrayList listB = new ArrayList();
        listB.add("wine");
        listB.add("coffee");

        Person p = new Person(20, "Alex", listA);
        Person q = new Person(20, "Craig", listB);

        Pair pair = comparer.getDiff(p, q);
        Assert.assertEquals("", pair.getKey());
        Assert.assertEquals( "{\"age\":20,\"name\":\"Craig\",\"likes\":[\"wine\",\"coffee\"]}", getJson(pair.getValue()) );
    }

    @org.junit.Test
    public void nestedObjectSendsPatchForField() {

        Coffee latte = new Coffee("Latte", 5);
        Coffee flatWhite = new Coffee("Flat White", 5);

        Person p = new Person(20, "Alex", latte);
        Person q = new Person(20, "Alex", flatWhite);

        Pair pair = comparer.getDiff(p, q);
        Assert.assertEquals("favouriteCoffee/name", pair.getKey());
        Assert.assertEquals("Flat White", pair.getValue());
    }

    @org.junit.Test
    public void nestedObjectSendsPatchForObject() {

        Coffee latte = new Coffee("Latte", 5);
        Coffee flatWhite = new Coffee("Flat White", 7);

        Person p = new Person(20, "Alex", latte);
        Person q = new Person(20, "Alex", flatWhite);

        Pair pair = comparer.getDiff(p, q);
        Assert.assertEquals("favouriteCoffee", pair.getKey());
        Assert.assertEquals( "{\"name\":\"Flat White\",\"price\":7}", getJson(pair.getValue()) );
    }

    private String getJson( Object obj ) {
        return gson.toJson( obj );
    }

    class Person {

        int age;
        String name;
        List likes;
        Coffee favouriteCoffee;

        Person(int age, String name, List likes) {
            this.age = age;
            this.name = name;
            this.likes = likes;
        }

        Person(int age, String name, Coffee favouriteCoffee) {
            this.age = age;
            this.name = name;
            this.favouriteCoffee = favouriteCoffee;
        }
    }

    class Coffee {

        String name;
        int price;

        Coffee(String name, int price) {
            this.name = name;
            this.price = price;
        }
    }
}

