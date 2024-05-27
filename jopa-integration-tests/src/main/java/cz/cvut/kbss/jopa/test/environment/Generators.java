/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Generators of test data.
 */
public abstract class Generators {

    private static final int DEFAULT_MIN = 5;
    private static final int DEFAULT_SIZE = 10;
    private static final Set<String> TYPES = getTypes();

    private static final String PROPERTY_URI_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#property";
    private static final String TYPE_URI_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#Entity";

    private static final Random RANDOM = new Random();

    private Generators() {
        // Private constructor
    }

    public static List<OWLClassA> createSimpleList() {
        return createSimpleList(randomPositiveInt(DEFAULT_MIN, DEFAULT_SIZE));
    }

    public static List<OWLClassA> createSimpleList(int size) {
        assert size > 0;
        final List<OWLClassA> lst = new ArrayList<>(size);
        generateInstances(lst, "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityASimple",
                size);
        return lst;
    }

    public static List<OWLClassA> createReferencedList() {
        return createReferencedList(randomPositiveInt(DEFAULT_MIN, DEFAULT_SIZE));
    }

    public static List<OWLClassA> createReferencedList(int size) {
        assert size > 0;
        final List<OWLClassA> lst = new ArrayList<>(size);
        generateInstances(lst,
                "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityAReferenced", size);
        return lst;
    }

    public static List<LocalDate> createDataPropertyList() {
        final List<LocalDate> list = new ArrayList<>(DEFAULT_SIZE);
        for (int i = DEFAULT_SIZE; i >= 0; i--) {
            list.add(LocalDate.now().minusDays(i));
        }
        return list;
    }

    public static List<OWLClassA> createRDFCollection(int size) {
        assert size > 0;
        final List<OWLClassA> lst = new ArrayList<>(size);
        generateInstances(lst,
                "http://krizik.felk.cvut.cz/ontologies/jopa/tests/rdf-collection-item", size);
        return lst;
    }

    public static List<URI> createListOfIdentifiers() {
        return createSimpleList().stream().map(OWLClassA::getUri).collect(Collectors.toList());
    }

    public static Set<OWLClassA> createSimpleSet() {
        return createSimpleSet(randomPositiveInt(DEFAULT_MIN, DEFAULT_SIZE));
    }

    public static Set<OWLClassA> createSimpleSet(int size) {
        assert size > 0;
        final Set<OWLClassA> set = new HashSet<>(size);
        generateInstances(set, "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityASimpleSet",
                size);
        return set;
    }

    public static Map<String, Set<String>> createProperties() {
        return createProperties(randomPositiveInt(DEFAULT_MIN, DEFAULT_SIZE));
    }

    public static Map<String, Set<String>> createProperties(int size) {
        assert size > 0;
        final Map<String, Set<String>> m = new HashMap<>(size);
        int counter = randomInt(1000);
        for (int i = 0; i < size; i++) {
            final Set<String> value = new HashSet<>(4);
            for (int j = 0; j < size; j++) {
                value.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/ObjectPropertyValue_" + j + "_"
                        + counter);
            }
            m.put(PROPERTY_URI_BASE + counter, value);
            counter++;

        }
        return m;
    }

    public static Map<URI, Set<Object>> createTypedProperties() {
        return createTypedProperties(randomPositiveInt(DEFAULT_MIN, DEFAULT_SIZE));
    }

    public static Map<URI, Set<Object>> createTypedProperties(int size) {
        assert size > 0;
        final Map<URI, Set<Object>> props = new HashMap<>(size);
        int counter = randomInt(1000);
        for (int i = 0; i < size; i++) {
            final Set<Object> value = new HashSet<>();
            for (int j = 0; j < size; j++) {
                // Generate either an individual's URI or random data value. But same type for a property
                // (so that the property is either object or data, but not both)
                if (counter % 2 == 0) {
                    value.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/Property_" + counter +
                            "Individual_" + j));
                } else {
                    value.add(generateRandomPropertyValue(j, counter));
                }
            }
            props.put(URI.create(PROPERTY_URI_BASE + counter), value);
            counter++;
        }
        return props;
    }

    private static Object generateRandomPropertyValue(int valueIndex, int propertyIndex) {
        final int random = randomInt(10);
        switch (random) {
            case 0: // boolean
                return valueIndex % 2 == 0;
            case 1: // int
                return valueIndex;
            case 2: // long
                return System.currentTimeMillis();
            case 3: //double
                return ((double) propertyIndex + 1) / (valueIndex + 1);
            case 4: // datetime
                // Generate date rounded to milliseconds to prevent issues with time rounding
                return OffsetDateTime.now().truncatedTo(ChronoUnit.MILLIS);
            case 5:
                return OffsetTime.now().truncatedTo(ChronoUnit.MILLIS);
            case 6:
                return LocalDate.now();
            case 7: // String
                return "TypedProperty_" + propertyIndex + "Value_" + valueIndex;
            case 8:
                return BigInteger.valueOf(valueIndex);
            case 9:
                return BigDecimal.valueOf(Math.PI);
            default:
                throw new IllegalArgumentException();
        }
    }

    private static void generateInstances(Collection<OWLClassA> col, String uriBase, int size) {
        assert size > 0;
        int counter = randomInt(1000);
        for (int i = 0; i < size; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create(uriBase + counter));
            a.setStringAttribute("stringAttributeeee" + counter);
            counter++;
            a.setTypes(TYPES);
            col.add(a);
        }
    }

    private static Set<String> getTypes() {
        final Set<String> types = new HashSet<>(3);
        types.add(Vocabulary.CLASS_IRI_BASE + "OWLClassDF");
        types.add(Vocabulary.CLASS_IRI_BASE + "OWLClassDFF");
        types.add(Vocabulary.CLASS_IRI_BASE + "OWLClassDFFF");
        return types;
    }

    public static Set<URL> createUrls() {
        return Generators.createSimpleList().stream().map(a -> {
            try {
                return a.getUri().toURL();
            } catch (MalformedURLException e) {
                throw new IllegalArgumentException(e);
            }
        }).collect(Collectors.toSet());
    }

    public static int randomInt() {
        return RANDOM.nextInt();
    }

    public static int randomInt(int max) {
        return RANDOM.nextInt(max);
    }

    /**
     * Gets a random int between {@code min} and {@code max}.
     *
     *
     * @param min lower bound (inclusive)
     * @param max upper bound (exclusive)
     * @return Random positive integer
     */
    public static int randomPositiveInt(int min, int max) {
        assert min >= 0;
        if (max <= min) {
            throw new IllegalArgumentException("Upper bound has to be greater than the lower bound.");
        }
        int rand;
        do {
            rand = RANDOM.nextInt(max);
        } while (rand < min);
        return rand;
    }

    public static boolean randomBoolean() {
        return RANDOM.nextBoolean();
    }

    public static Set<URI> createUriTypes() {
        final int count = randomPositiveInt(DEFAULT_MIN, DEFAULT_SIZE);
        final Set<URI> result = new HashSet<>();
        for (int i = 0; i < count; i++) {
            result.add(URI.create(TYPE_URI_BASE + randomInt(Integer.MAX_VALUE)));
        }
        return result;
    }

    /**
     * Generates a random URI.
     *
     * @return Random URI
     */
    public static URI generateUri() {
        return URI.create(Vocabulary.INDIVIDUAL_IRI_BASE + randomInt(Integer.MAX_VALUE));
    }

    /**
     * Gets random item from the specified list.
     * @param items List of items
     * @return Random item from the list
     */
    public static <T> T getRandomItem(List<T> items) {
        return items.get(randomPositiveInt(0, items.size()));
    }

    public static Random getRandomGenerator() {
        return RANDOM;
    }
}
