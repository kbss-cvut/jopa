/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;

import java.net.URI;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Generators of test data.
 */
public abstract class Generators {

    public static final String LANG = "en";

    public static final int DEFAULT_SIZE = 5;

    private static final Random RAND = new Random();

    private static final Set<String> TYPES = generateTypes(DEFAULT_SIZE);

    private Generators() {
        // Private constructor
    }

    public static int randomInt() {
        return RAND.nextInt();
    }

    public static int randomInt(int max) {
        return RAND.nextInt(max);
    }

    /**
     * Gets a random integer greater than one.
     *
     * @param max Upper bound
     * @return Random integer
     */
    public static int randomPositiveInt(int max) {
        assert max > 1;
        int res;
        do {
            res = RAND.nextInt(max);
        } while (res <= 1);
        return res;
    }

    public static boolean randomBoolean() {
        return RAND.nextBoolean();
    }

    public static URI createIndividualIdentifier() {
        return URI.create(Vocabulary.INDIVIDUAL_BASE + randomInt(Integer.MAX_VALUE));
    }

    public static URI createPropertyIdentifier() {
        return URI.create(Vocabulary.ATTRIBUTE_BASE + randomInt(Integer.MAX_VALUE));
    }

    public static Map<String, Set<String>> generateStringProperties() {
        return generateStringProperties(DEFAULT_SIZE, DEFAULT_SIZE);
    }

    public static Map<String, Set<String>> generateStringProperties(int propCount, int valCount) {
        int valueCounter = 0;
        final Map<String, Set<String>> properties = new HashMap<>(propCount);
        for (int i = 0; i < propCount; i++) {
            final Set<String> values = new HashSet<>(valCount);
            properties.put(Vocabulary.ATTRIBUTE_BASE + i, values);
            boolean objectProperty = i % 2 != 0;
            for (int j = 0; j < valCount; j++) {
                if (objectProperty) {
                    values.add(Vocabulary.INDIVIDUAL_BASE + "value_" + valueCounter++);
                } else {
                    values.add(j % 2 != 0 ? "value" + valueCounter++ : Integer.toString(valueCounter++));
                }
            }
        }
        return properties;
    }

    public static Map<URI, Set<Object>> generateTypedProperties() {
        return generateTypedProperties(DEFAULT_SIZE, DEFAULT_SIZE);
    }

    public static Map<URI, Set<Object>> generateTypedProperties(int propCount, int valCount) {
        int valueCounter = 0;
        final Map<URI, Set<Object>> properties = new HashMap<>(propCount);
        for (int i = 0; i < propCount; i++) {
            final Set<Object> values = new HashSet<>(valCount);
            properties.put(URI.create(Vocabulary.ATTRIBUTE_BASE + i), values);
            boolean objectProperty = i % 2 != 0;
            for (int j = 0; j < valCount; j++) {
                if (objectProperty) {
                    values.add(URI.create(Vocabulary.INDIVIDUAL_BASE + "value_" + valueCounter++));
                } else {
                    values.add(generateDataPropertyValue(j));
                }
            }
        }
        return properties;
    }

    private static Object generateDataPropertyValue(int counter) {
        final int index = counter % 5;
        return switch (index) {
            case 0 -> "StringValue_" + counter;
            case 1 -> true;
            case 2 -> counter;
            case 3 -> (double) counter;
            case 4 -> new Date();
            default -> null;
        };
    }

    public static OWLClassA generateOwlClassAInstance() {
        final OWLClassA a = new OWLClassA(createIndividualIdentifier());
        a.setStringAttribute("stringAttribute" + RAND.nextInt());
        a.setTypes(TYPES);
        return a;
    }

    public static List<OWLClassA> generateInstances(int size) {
        assert size > 0;
        final List<OWLClassA> lst = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            lst.add(generateOwlClassAInstance());
        }
        return lst;
    }

    public static Set<String> generateTypes(int count) {
        final Set<String> types = new HashSet<>(count);
        for (int i = 0; i < count; i++) {
            types.add(Vocabulary.CLASS_BASE + RAND.nextInt(10000));
        }
        return types;
    }

    public static Set<URI> generateUriTypes(int count) {
        return generateTypes(count).stream().map(URI::create).collect(Collectors.toSet());
    }
}
