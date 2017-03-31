package io.deepstream;


import com.google.gson.*;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URISyntaxException;

public class JSONPathTest {
    private Gson gson = new Gson();
    private UtilJSONPath jsonPath;
    private JsonObject coreElement;

    @Before
    public void setUp() throws URISyntaxException {
        JsonArray pastAddresses = new JsonArray();

        JsonObject address1 = new JsonObject();
        address1.addProperty( "street", "firststreet" );
        address1.addProperty( "postCode",1001 );
        pastAddresses.add( address1 );

        JsonObject address2 = new JsonObject();
        address2.addProperty( "street", "secondstreet" );
        address2.addProperty( "postCode",2002 );
        pastAddresses.add( address2 );

        JsonObject currentAddress = new JsonObject();
        currentAddress.addProperty( "street", "currentStreet" );

        coreElement = new JsonObject();
        coreElement.addProperty( "firstname", "yasser" );
        coreElement.addProperty( "lastname", "fadl" );
        coreElement.addProperty( "1234", "integer index" );
        coreElement.add( "address", currentAddress );
        coreElement.add( "pastAddresses", pastAddresses );

        jsonPath = new UtilJSONPath( coreElement );
    }

    @After
    public void tearDown() {

    }

    /**
     * Get
     */

    @Test
    public void retrievesSimplePaths() {
        Assert.assertEquals( "yasser", jsonPath.get( "firstname" ).getAsString() );
    };

    @Test
    public void retrievesNestedPaths() {
        Assert.assertEquals( "currentStreet", jsonPath.get( "address.street" ).getAsString() );
    };

    @Test
    public void retrievesArrayEntries() {
        Assert.assertEquals( "secondstreet", jsonPath.get( "pastAddresses[1].street" ).getAsString() );
    };

    @Test
    public void retrievesOtherArrayEntries() {
        Assert.assertEquals( 1001, jsonPath.get( "pastAddresses[0].postCode" ).getAsInt() );
    };

    @Test
    public void handlesWhitespaces() {
        Assert.assertEquals( 1001, jsonPath.get( "pastAddresses[ 0 ].postCode" ).getAsInt() );
    };

    @Test
    public void returnsNullForNonExistingKeys() {
        Assert.assertNull( jsonPath.get( "doesNotExist" ) );
    };

    @Test
    public void returnsNullForNonExistingNestedKeys() {
        Assert.assertNull( jsonPath.get( "address.number" ) );
    };

    @Test
    public void returnsNullForOutOfBoundIndices() {
        Assert.assertNull( jsonPath.get( "pastAddresses[ 8 ]" ) );
    };

    @Test
    public void returnsNullForUndefinedKeys() {
        Assert.assertNull( jsonPath.get( "pastAddresses[ -1 ]" ) );
    };

    /**
     * Set
     */

    @Test
    public void setsSimpleValues() {
        jsonPath.set( "firstname", gson.toJsonTree( "wolfram" ) );
        Assert.assertEquals( "wolfram", coreElement.get( "firstname" ).getAsString() );
    };

    @Test
    public void setsNestedValues() {
        jsonPath.set( "address.street", gson.toJsonTree( "someStreet" ) );

        JsonObject expected = new JsonObject();
        expected.addProperty( "street", "someStreet" );

        Assert.assertEquals( expected, coreElement.get( "address" ) );
    };

    @Test
    public void setsValuesForArrays() {
        jsonPath.set( "pastAddresses[1].street", gson.toJsonTree( "someStreet" ) );

        JsonObject expected = new JsonObject();
        expected.addProperty( "street", "someStreet" );
        expected.addProperty( "postCode", 2002 );

        Assert.assertEquals(
                expected,
                coreElement.get( "pastAddresses" ).getAsJsonArray().get(1)
        );
    };

    @Test
    public void setsValuesForArrayIndexes() {
        JsonObject newAddress = new JsonObject();
        newAddress.addProperty( "street", "anotherStreet" );
        newAddress.addProperty( "postCode", 300 );

        jsonPath.set( "pastAddresses[2]", newAddress );
        Assert.assertEquals(
                newAddress,
                coreElement.get( "pastAddresses" ).getAsJsonArray().get(2)
        );
    };

    @Test
    public void createsNonExistentPathWhenSetting() {
        jsonPath.set("nested.property.created", gson.toJsonTree("some value"));
        Assert.assertEquals(
                "some value",
                jsonPath.get("nested.property.created").getAsString()
        );
    }

    @Test
    public void deletesSimpleValues() {
        Assert.assertEquals(
                "yasser",
                coreElement.get( "firstname" ).getAsString()
        );
        jsonPath.delete( "firstname" );
        Assert.assertTrue(
                jsonPath.get( "firstname" ) == null
        );
    };

    @Test
    public void deletesNestedSimpleValues() {
        Assert.assertEquals( "currentStreet", jsonPath.get( "address.street" ).getAsString() );
        jsonPath.delete( "address.street" );
        Assert.assertTrue(
                jsonPath.get( "address.street" ) == null
        );
    };

    @Test
    public void deletesObjectValuesFromArrays() {
        JsonObject expected = new JsonObject();
        expected.addProperty( "street", "secondstreet" );
        expected.addProperty( "postCode",2002 );
        Assert.assertEquals( expected, jsonPath.get( "pastAddresses[1]" ) );

        jsonPath.delete( "pastAddresses[1]" );
        Assert.assertFalse(
                jsonPath.get( "pastAddresses" ).getAsJsonArray().contains( expected )
        );
    };

    @Test
    public void deletesObjectValueFromArray() {
        JsonObject expected = new JsonObject();
        expected.addProperty( "street", "secondstreet" );
        expected.addProperty( "postCode",2002 );
        Object o = jsonPath.get("pastAddresses[1]");
        Assert.assertEquals( expected, jsonPath.get( "pastAddresses[1]" ) );

        jsonPath.delete( "pastAddresses[1]" );
        Assert.assertFalse(
                jsonPath.get( "pastAddresses" ).getAsJsonArray().contains( expected )
        );
    };

    @Test
    public void extendsExisitingObjects() {
        jsonPath.set( "randomKey", gson.toJsonTree( "randomValue" ) );
        Assert.assertEquals( "randomValue", coreElement.get( "randomKey" ).getAsString() );
    }

    @Test
    public void extendsExisitingArrays() {
        jsonPath.set( "randomKey[ 0 ].name", gson.toJsonTree( "randomValue" ) );
        Assert.assertEquals(
                "randomValue",
                coreElement.get( "randomKey" ).getAsJsonArray().get( 0 ).getAsJsonObject().get( "name" ).getAsString()
        );
    }
}
