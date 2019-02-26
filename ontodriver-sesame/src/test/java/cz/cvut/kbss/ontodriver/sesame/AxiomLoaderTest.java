/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.config.Constants;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.environment.TestRepositoryProvider;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class AxiomLoaderTest {

    private TestRepositoryProvider repositoryProvider = new TestRepositoryProvider();

    private Generator.GeneratedData generatedData;
    private ValueFactory vf;

    private Connector connector;

    private AxiomLoader axiomLoader;

    @Before
    public void setUp() throws Exception {
        this.connector = repositoryProvider.createConnector(false);
        final Repository repository = connector.unwrap(Repository.class);
        this.axiomLoader = new AxiomLoader(connector, repository.getValueFactory(), "en");
        this.generatedData = Generator.initTestData(repository);
        this.vf = connector.unwrap(Repository.class).getValueFactory();
    }

    @After
    public void tearDown() throws Exception {
        connector.close();
        repositoryProvider.close();
    }

    @Test
    public void loadAxiomsForSingleAssertionFindsMatchingValues() throws Exception {
        connector.begin();
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final Set<Object> values = generatedData.values.get(individual).get(property);

        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(values.size(), res.size());
        for (Axiom a : res) {
            assertEquals(individual, a.getSubject().toString());
            assertEquals(property, a.getAssertion());
            assertTrue(values.contains(a.getValue().getValue()));
        }
    }

    @Test
    public void loadAxiomsReturnsEmptyCollectionForUnknownIndividual() throws Exception {
        connector.begin();
        final String individual = "http://krizik.felk.cvut.cz/ontologies/sesame/individuals#Unknown";
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(Assertion.createClassAssertion(false));

        assertTrue(axiomLoader.loadAxioms(desc).isEmpty());
    }

    @Test
    public void loadAxiomsLoadsAxiomsBasedOnSpecifiedAssertions() throws Exception {
        connector.begin();
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        generatedData.values.get(individual).keySet().forEach(desc::addAssertion);

        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(generatedData.getTotalValueCount(individual), res.size());
    }

    @Test
    public void loadAxiomsSkipsValuesOfNonSelectedAssertions() throws Exception {
        connector.begin();
        final String individual = selectIndividualWithEnoughProperties();
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        final Set<Assertion> skippedAssertions = new HashSet<>();
        int counter = 0;
        for (Assertion a : generatedData.values.get(individual).keySet()) {
            if (counter < Constants.DEFAULT_LOAD_ALL_THRESHOLD) {
                desc.addAssertion(a);
            } else {
                skippedAssertions.add(a);
            }
            counter++;
        }

        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        res.forEach(a -> assertFalse(skippedAssertions.contains(a.getAssertion())));
    }

    private String selectIndividualWithEnoughProperties() {
        for (String individual : generatedData.individuals) {
            if (generatedData.values.get(individual).keySet().size() > Constants.DEFAULT_LOAD_ALL_THRESHOLD) {
                return individual;
            }
        }
        Assert.fail("Individual with sufficient number of assertions not found.");
        return null;
    }

    @Test
    public void loadsAllAxiomsWhenUnspecifiedPropertyIsUsedInDescriptor() throws Exception {
        connector.begin();
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        // Add some random assertions. They are not that important, because the unspecified property will cover everything
        generatedData.values.get(individual).keySet().stream().filter(a -> Generator.randomBoolean())
                            .forEach(desc::addAssertion);

        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(generatedData.getTotalValueCount(individual), res.size());
    }

    @Test
    public void loadAxiomsSearchesInContextWhenItIsSpecifiedForAssertion() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final String context = "http://krizik.felk.cvut.cz/contextOne";
        final Object value = saveValueIntoContext(individual, property, context);

        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.setAssertionContext(property, URI.create(context));
        connector.begin();
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(1, res.size());
        final Axiom<?> a = res.iterator().next();
        assertEquals(property, a.getAssertion());
        assertEquals(value.toString(), a.getValue().getValue().toString());
    }

    private Object saveValueIntoContext(String individual, Assertion property, String context) throws Exception {
        final RepositoryConnection conn = connector.unwrap(Repository.class).getConnection();
        conn.begin();
        final Object value;
        if (property.getType() == Assertion.AssertionType.DATA_PROPERTY) {
            value = new Date();
            conn.add(vf.createIRI(individual), vf.createIRI(property.getIdentifier().toString()),
                    vf.createLiteral((Date) value), vf.createIRI(context));

        } else {
            value = NamedResource.create("http://krizik.felk.cvut.cz/individualInContext" + Generator.randomInt());
            conn.add(vf.createIRI(individual), vf.createIRI(property.getIdentifier().toString()),
                    vf.createIRI(value.toString()), vf.createIRI(context));
        }
        conn.commit();
        conn.close();
        return value;
    }

    @Test
    public void loadAxiomsLoadsValuesFromContextAndNotFromDefaultWhenContextIsSpecifiedForAssertion() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final String context = "http://krizik.felk.cvut.cz/contextOne";
        final Object value = saveValueIntoContext(individual, property, context);
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.setAssertionContext(property, URI.create(context));
        final Set<Assertion> assertions = generatedData.values.get(individual).keySet();
        assertions.stream().filter(a -> !a.equals(property)).forEach(desc::addAssertion);

        connector.begin();
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        int valueCount = 1; // The value in context
        for (Assertion a : desc.getAssertions()) {
            if (!a.equals(property)) {
                valueCount += generatedData.values.get(individual).get(a).size();
            }
        }
        assertEquals(valueCount, res.size());
        for (Axiom<?> a : res) {
            assertTrue(assertions.contains(a.getAssertion()));
            if (a.getAssertion().equals(property)) {
                assertEquals(value, a.getValue().getValue());
            }
        }
    }

    @Test
    public void loadAxiomsCombinesUnspecifiedPropertyInDefaultWithPropertyInContext() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final String context = "http://krizik.felk.cvut.cz/contextOne";
        Object value = saveValueIntoContext(individual, property, context);
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.setAssertionContext(property, URI.create(context));
        final Assertion unspecified = Assertion.createUnspecifiedPropertyAssertion(false);
        desc.addAssertion(unspecified);

        connector.begin();
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(generatedData.getTotalValueCount(individual) + 1, res.size());
        final Optional<Axiom<?>> found = res.stream().filter(a ->
                a.getAssertion().equals(property) && a.getValue().getValue().equals(value)).findFirst();
        assertTrue(found.isPresent());
    }

    @Test
    public void loadAxiomsCombinesPropertiesInDefaultAndUnspecifiedPropertyInContext() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final Assertion propertyInCtx = Assertion
                .createDataPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/propertyInCtx"), false);
        final String context = "http://krizik.felk.cvut.cz/contextOne";
        int ctxCount = Generator.randomPositiveInt(10);
        final Set<Object> contextValues = new HashSet<>();
        for (int i = 0; i < ctxCount; i++) {
            contextValues.add(saveValueIntoContext(individual, propertyInCtx, context));
        }
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        desc.setAssertionContext(Assertion.createUnspecifiedPropertyAssertion(false), URI.create(context));

        connector.begin();
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(contextValues.size() + generatedData.values.get(individual).get(property).size(), res.size());
        for (Axiom<?> ax : res) {
            if (ax.getAssertion().equals(property)) {
                assertTrue(generatedData.values.get(individual).get(property).contains(ax.getValue().getValue()));
            } else {
                assertEquals(propertyInCtx.getIdentifier(), ax.getAssertion().getIdentifier());
                assertEquals(propertyInCtx.isInferred(), ax.getAssertion().isInferred());
                assertTrue(contextValues.contains(ax.getValue().getValue()));
            }
        }
    }

    @Test
    public void loadAxiomsSkipsPropertyValueOfInvalidType_OP() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion a = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/objectProperty"), false);
        final RepositoryConnection conn = connector.unwrap(Repository.class).getConnection();
        conn.begin();
        conn.add(vf.createIRI(individual), vf.createIRI(a.getIdentifier().toString()), vf.createLiteral(false));
        conn.commit();
        conn.close();

        connector.begin();
        final AxiomDescriptor descOne = new AxiomDescriptor(NamedResource.create(individual));
        descOne.addAssertion(a);
        assertTrue(axiomLoader.loadAxioms(descOne).isEmpty());
        final AxiomDescriptor descTwo = new AxiomDescriptor(NamedResource.create(individual));
        descTwo.addAssertion(Assertion.createDataPropertyAssertion(a.getIdentifier(), false));
        assertFalse(axiomLoader.loadAxioms(descTwo).isEmpty());
    }

    @Test
    public void loadAxiomsSkipsPropertyValueOfInvalidType_DP() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion a = Assertion
                .createDataPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/dataProperty"), false);
        final RepositoryConnection conn = connector.unwrap(Repository.class).getConnection();
        conn.begin();
        conn.add(vf.createIRI(individual), vf.createIRI(a.getIdentifier().toString()),
                vf.createIRI("http://krizik.felk.cvut.cz/individual"));
        conn.commit();
        conn.close();

        connector.begin();
        final AxiomDescriptor descOne = new AxiomDescriptor(NamedResource.create(individual));
        descOne.addAssertion(a);
        assertTrue(axiomLoader.loadAxioms(descOne).isEmpty());
        final AxiomDescriptor descTwo = new AxiomDescriptor(NamedResource.create(individual));
        descTwo.addAssertion(Assertion.createObjectPropertyAssertion(a.getIdentifier(), false));
        assertFalse(axiomLoader.loadAxioms(descTwo).isEmpty());
    }

    @Test
    public void loadAxiomsLoadsStringLiteralWithCorrectLanguage() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        // Language is en
        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        descriptor
                .addAssertion(Assertion.createAnnotationPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), false));
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(1, axioms.size());
        final Axiom<?> ax = axioms.iterator().next();
        assertEquals("a", ax.getValue().getValue());
    }

    private void persistLanguageTaggedStrings(String individual) throws OntoDriverException {
        final RepositoryConnection conn = connector.unwrap(Repository.class).getConnection();
        conn.begin();
        conn.add(vf.createStatement(vf.createIRI(individual), RDFS.LABEL, vf.createLiteral("a", "en")));
        conn.add(vf.createStatement(vf.createIRI(individual), RDFS.LABEL, vf.createLiteral("b", "cs")));
        conn.commit();
        conn.close();
    }

    @Test
    public void loadAxiomsLoadsStringLiteralWithCorrectLanguageForUnspecifiedPropertyType() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        // Language is en
        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        descriptor.addAssertion(Assertion.createPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), false));
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(1, axioms.size());
        final Axiom<?> ax = axioms.iterator().next();
        assertEquals("a", ax.getValue().getValue());
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInAssertion() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        final Assertion assertion =
                Assertion.createDataPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), "cs", false);
        descriptor.addAssertion(assertion);
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(1, axioms.size());
        final Axiom<?> ax = axioms.iterator().next();
        assertEquals("b", ax.getValue().getValue());
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInAssertionOfUnspecifiedProperty() throws
                                                                                                            Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        final Assertion assertion =
                Assertion.createPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), "cs", false);
        descriptor.addAssertion(assertion);
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(1, axioms.size());
        final Axiom<?> ax = axioms.iterator().next();
        assertEquals("b", ax.getValue().getValue());
    }

    @Test
    public void loadsStringLiteralWithAllLanguagesWhenLanguageTagIsExplicitlySetToNull() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        final Assertion assertion =
                Assertion.createDataPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), null, false);
        descriptor.addAssertion(assertion);
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(2, axioms.size());
        final Set<String> values = axioms.stream().map(ax -> ax.getValue().stringValue()).collect(Collectors.toSet());
        assertTrue(values.contains("a"));
        assertTrue(values.contains("b"));
    }
}