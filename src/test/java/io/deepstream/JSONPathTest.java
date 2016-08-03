package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
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
