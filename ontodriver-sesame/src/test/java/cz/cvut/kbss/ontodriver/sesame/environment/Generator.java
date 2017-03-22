/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.environment;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

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
            final String cls = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClass" + RAND.nextInt();
            data.classes.add(cls);
            int individualCount = randomPositiveInt(20);
            for (int j = 0; j < individualCount; j++) {
                final String ind = cls + "_instance-" + RAND.nextInt();
                data.individuals.add(ind);
                final IRI indUri = vf.createIRI(ind);
                generatePropertiesWithValues(indUri, data, connection);
                connection.add(indUri, RDF.TYPE, vf.createIRI(cls));
                data.addValue(ind, Assertion.createClassAssertion(false), java.net.URI.create(cls));
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
            final String property = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#property" + RAND.nextInt();
            final IRI propertyUri = vf.createIRI(property);
            final int valueCount = randomPositiveInt(5);
            final boolean isOp = RAND.nextBoolean();
            for (int j = 0; j < valueCount; j++) {
                if (isOp) {
                    final Assertion a = Assertion.createObjectPropertyAssertion(java.net.URI.create(property), false);
                    final String object =
                            "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassXY_instance" + RAND.nextInt();
                    connection.add(individual, propertyUri, vf.createIRI(object));
                    data.addValue(individual.toString(), a, NamedResource.create(object));
                } else {
                    final Assertion a = Assertion.createDataPropertyAssertion(java.net.URI.create(property), false);
                    final String value = "RandomValue" + RAND.nextInt();
                    connection.add(individual, propertyUri, vf.createLiteral(value, "en"));
                    data.addValue(individual.toString(), a, value);
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
        int index;
        do {
            index = RAND.nextInt(col.size());
        } while (index < 0);
        return index;
    }

    public static boolean randomBoolean() {
        return RAND.nextBoolean();
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
