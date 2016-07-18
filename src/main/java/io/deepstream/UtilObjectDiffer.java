package io.deepstream;

import com.google.gson.Gson;
import javafx.util.Pair;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;


class UtilObjectDiffer {

    StringBuilder path = new StringBuilder();
    Gson gson = new Gson();
    String value;

    String oldDiffNodeName;
    Object oldDiffNodeValue;

    String newDiffNodeName;
    Object newDiffNodeValue;

    /**
     * Gets the difference between two objects.
     *
     * Given the json representation of two POJOs:
     *
     * old = {
     *     name: "Marge",
     *     age: 30,
     *     favouriteCoffee: {
     *         name: "Flat White",
     *         price: 6
     *     }
     * }
     *
     * new = {
     *     name: "Marge",
     *     age: 30,
     *     favouriteCoffee: {
     *         name: "Latte",
     *         price: 6
     *     }
     * }
     *
     * The following would be returned in a Pair:
     *
     * ( "favouriteCoffee/name", "Latte" )
     *
     * @param oldNode the old version of the object
     * @param newNode the new version of the object
     * @return a Pair<String,Object> with the path of the changes and the actual changes
     */
    //todo: investigate returning an object to use with MessageBuilder.typed()
    Pair getDiff(Object oldNode, Object newNode) {
        boolean foundDifference = false;

        // No change
        if(oldNode.equals(newNode)) {
            System.out.println("Same object, exiting");
            return new Pair(path.toString(), value);
        }

        // Whole new class todo: tests for this
        if(oldNode.getClass() != newNode.getClass()) {
            System.out.println("Not same class, exiting");
            return new Pair(path.toString(), value);
        }

        // Iterate over fields and compare
        //todo: investigate sorting for these fields
        Field[] oldFields = oldNode.getClass().getDeclaredFields();
        Field[] newFields = newNode.getClass().getDeclaredFields();
        try
        {
            for (int i = 0; i < oldFields.length; i++) {
                Object oldNodeValue = oldFields[i].get(oldNode);
                String oldNodeName = oldFields[i].getName();
                Object currentNodeValue = newFields[i].get(newNode);
                String currentNodeName = newFields[i].getName();

                if( oldNodeValue == null && currentNodeValue == null ) {
                    continue;
                }

                System.out.printf("Old %s is: %s. New %s is: %s\n", oldNodeName, oldNodeValue, currentNodeName, currentNodeValue);

                if( !oldNodeValue.equals(currentNodeValue) ) {

                    if( foundDifference ) {
                        System.out.println("More than 1 item is different. BREAK");
                        return new Pair(path.toString(), gson.toJson(newNode));
                    }

                    // Set flags for when only one attribute is different
                    System.out.println("They are different");
                    foundDifference = true;
                    oldDiffNodeName = oldNodeName;
                    oldDiffNodeValue = oldNodeValue;
                    newDiffNodeName = currentNodeName;
                    newDiffNodeValue = currentNodeValue;
                }
            }

            System.out.println("Didn't find two fields different");

            // Only one attribute in the node is different
            // either recursively getDiff the different attribute, or
            // return if a primitive
            if( newDiffNodeValue != null ) {
                System.out.println("Found one field different");
                buildPath( newDiffNodeName );

                if(  newDiffNodeValue instanceof Object && (
                        !(newDiffNodeValue instanceof String) &&
                                !(newDiffNodeValue instanceof List) &&
                                !(newDiffNodeValue instanceof Map) ) ) //todo: implement functionality for maps
                {
                    System.out.println("Going deeper...");
                    return getDiff(oldDiffNodeValue, newDiffNodeValue);
                }

                return new Pair( path.toString(), newDiffNodeValue.toString() );
            }
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
        }
        return new Pair(path.toString(), value);
    }

    private void buildPath(String currentNodeName) {
        if( path.toString().equals("") ) {
            path.append( currentNodeName );
        } else {
            path.append( "/" );
            path.append( currentNodeName );
        }
    }
}