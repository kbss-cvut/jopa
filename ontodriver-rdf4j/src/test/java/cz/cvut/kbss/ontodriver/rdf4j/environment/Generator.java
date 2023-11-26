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
package cz.cvut.kbss.ontodriver.rdf4j.environment;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.net.URI;
import java.util.*;

public class Generator {

    private static final Random RAND = new Random();

    public static GeneratedData initTestData(Repository repository) throws RepositoryException {
        final RepositoryConnection connection = repository.getConnection();
        connection.begin();
        final ValueFactory vf = connection.getValueFactory();

        final GeneratedData data = new GeneratedData();
        int classCount = randomPositiveInt(10);
        for (int i = 0; i < classCount; i++) {
            final String cls = Vocabulary.CLASS_IRI_BASE + RAND.nextInt();
            data.classes.add(cls);
            int individualCount = randomPositiveInt(20);
            for (int j = 0; j < individualCount; j++) {
                final String ind = cls + "_instance-" + RAND.nextInt();
                data.individuals.add(ind);
                final IRI indUri = vf.createIRI(ind);
                generatePropertiesWithValues(indUri, data, connection);
                connection.add(indUri, RDF.TYPE, vf.createIRI(cls));
                data.addValue(ind, Assertion.createClassAssertion(false), URI.create(cls));
            }
        }

        connection.commit();
        connection.close();
        return data;
    }

    private static void generatePropertiesWithValues(IRI individual, GeneratedData data,
                                                     RepositoryConnection connection) throws RepositoryException {
        final ValueFactory vf = connection.getValueFactory();
        final int propCount = randomPositiveInt(20);
        for (int i = 0; i < propCount; i++) {
            final String property = Vocabulary.PROPERTY_IRI_BASE + RAND.nextInt();
            final IRI propertyUri = vf.createIRI(property);
            final int valueCount = randomPositiveInt(5);
            final boolean isOp = RAND.nextBoolean();
            for (int j = 0; j < valueCount; j++) {
                if (isOp) {
                    final Assertion a = Assertion.createObjectPropertyAssertion(URI.create(property), false);
                    final String object = Vocabulary.INDIVIDUAL_IRI_BASE + RAND.nextInt();
                    connection.add(individual, propertyUri, vf.createIRI(object));
                    data.addValue(individual.toString(), a, NamedResource.create(object));
                } else {
                    final Assertion a = Assertion.createDataPropertyAssertion(URI.create(property), false);
                    final String value = "RandomValue" + RAND.nextInt();
                    connection.add(individual, propertyUri, vf.createLiteral(value, "en"));
                    data.addValue(individual.toString(), a, new LangString(value, "en"));
                }
            }
        }
    }

    public static int randomInt() {
        return RAND.nextInt();
    }

    public static int randomPositiveInt(int max) {
        int res;
        do {
            res = RAND.nextInt(max);
        } while (res <= 1);
        return res;
    }

    public static int randomIndex(Collection<?> col) {
        return RAND.nextInt(col.size());
    }

    public static boolean randomBoolean() {
        return RAND.nextBoolean();
    }

    public static URI generateUri() {
        return URI.create(Vocabulary.INDIVIDUAL_IRI_BASE + randomInt());
    }

    public static class GeneratedData {

        public final List<String> classes = new ArrayList<>();
        public final List<String> individuals = new ArrayList<>();
        public final Map<String, Map<Assertion, Set<Object>>> values = new HashMap<>();

        private void addValue(String individual, Assertion property, Object value) {
            if (!values.containsKey(individual)) {
                values.put(individual, new HashMap<>());
            }
            final Map<Assertion, Set<Object>> propertyValues = values.get(individual);
            if (!propertyValues.containsKey(property)) {
                propertyValues.put(property, new HashSet<>());
            }
            final Set<Object> vals = propertyValues.get(property);
            vals.add(value);
        }

        /**
         * Gets total number of values of all properties of the specified individual.
         *
         * @param individual Individual URI
         * @return Value count
         */
        public int getTotalValueCount(String individual) {
            if (!values.containsKey(individual)) {
                return 0;
            }
            final Map<Assertion, Set<Object>> propertyValues = values.get(individual);
            int cnt = 0;
            for (Set<Object> set : propertyValues.values()) {
                cnt += set.size();
            }
            return cnt;
        }
    }
}
