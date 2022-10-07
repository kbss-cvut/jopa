/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.environment.TestRepositoryProvider;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

class AxiomLoaderTest {

    private final TestRepositoryProvider repositoryProvider = new TestRepositoryProvider();

    private Generator.GeneratedData generatedData;
    private final ValueFactory vf = SimpleValueFactory.getInstance();

    private Connector connector;

    private AxiomLoader axiomLoader;

    @BeforeEach
    void setUp() throws Exception {
        this.connector = repositoryProvider.createConnector(false);
        final Repository repository = connector.unwrap(Repository.class);
        final DriverConfiguration driverConfig = new DriverConfiguration(TestRepositoryProvider.storageProperties());

        this.axiomLoader = new AxiomLoader(connector, new RuntimeConfiguration(driverConfig));
        this.generatedData = Generator.initTestData(repository);
    }

    @AfterEach
    void tearDown() throws Exception {
        connector.close();
        repositoryProvider.close();
    }

    @Test
    void loadAxiomsForSingleAssertionFindsMatchingValues() throws Exception {
        connector.begin();
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final Set<Object> values = generatedData.values.get(individual).get(property);

        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(values.size(), res.size());
        for (Axiom<?> a : res) {
            assertEquals(individual, a.getSubject().toString());
            assertEquals(property, a.getAssertion());
            assertTrue(values.contains(a.getValue().getValue()));
        }
    }

    @Test
    void loadAxiomsReturnsEmptyCollectionForUnknownIndividual() throws Exception {
        connector.begin();
        final String individual = Generator.generateUri().toString();
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(Assertion.createClassAssertion(false));

        assertTrue(axiomLoader.loadAxioms(desc).isEmpty());
    }

    @Test
    void loadAxiomsLoadsAxiomsBasedOnSpecifiedAssertions() throws Exception {
        connector.begin();
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        generatedData.values.get(individual).keySet().forEach(desc::addAssertion);

        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(generatedData.getTotalValueCount(individual), res.size());
    }

    @Test
    void loadAxiomsSkipsValuesOfNonSelectedAssertions() throws Exception {
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
        fail("Individual with sufficient number of assertions not found.");
        return null;
    }

    @Test
    void loadsAllAxiomsWhenUnspecifiedPropertyIsUsedInDescriptor() throws Exception {
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
    void loadAxiomsSearchesInContextWhenItIsSpecifiedForAssertion() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final String context = Generator.generateUri().toString();
        final Object value = saveValueIntoContext(individual, property, context);

        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.addAssertionContext(property, URI.create(context));
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
            value = Generator.randomInt();
            conn.add(vf.createIRI(individual), vf.createIRI(property.getIdentifier().toString()),
                    vf.createLiteral((Integer) value), vf.createIRI(context));

        } else {
            value = property.getType() == Assertion.AssertionType.CLASS ? Generator.generateUri() : NamedResource.create(Generator.generateUri());
            conn.add(vf.createIRI(individual), vf.createIRI(property.getIdentifier().toString()),
                    vf.createIRI(value.toString()), vf.createIRI(context));
        }
        conn.commit();
        conn.close();
        return value;
    }

    @Test
    void loadAxiomsLoadsValuesFromContextAndNotFromDefaultWhenContextIsSpecifiedForAssertion() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final String context = Generator.generateUri().toString();
        final Object value = saveValueIntoContext(individual, property, context);
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.addAssertionContext(property, URI.create(context));
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
    void loadAxiomsCombinesUnspecifiedPropertyInDefaultWithPropertyInContext() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final String context = Generator.generateUri().toString();
        Object value = saveValueIntoContext(individual, property, context);
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.addAssertionContext(property, URI.create(context));
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
    void loadAxiomsCombinesPropertiesInDefaultAndUnspecifiedPropertyInContext() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final Assertion propertyInCtx = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final String context = Generator.generateUri().toString();
        int ctxCount = Generator.randomPositiveInt(10);
        final Set<Object> contextValues = new HashSet<>();
        for (int i = 0; i < ctxCount; i++) {
            contextValues.add(saveValueIntoContext(individual, propertyInCtx, context));
        }
        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.addAssertion(Assertion.createUnspecifiedPropertyAssertion(false));
        desc.addAssertionContext(Assertion.createUnspecifiedPropertyAssertion(false), URI.create(context));

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
    void loadAxiomsSkipsPropertyValueOfInvalidType_OP() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion a = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
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
    void loadAxiomsSkipsPropertyValueOfInvalidType_DP() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion a = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final RepositoryConnection conn = connector.unwrap(Repository.class).getConnection();
        conn.begin();
        conn.add(vf.createIRI(individual), vf.createIRI(a.getIdentifier().toString()),
                vf.createIRI(Generator.generateUri().toString()));
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
    void loadAxiomsLoadsStringLiteralWithCorrectLanguage() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        // Language is en
        descriptor
                .addAssertion(
                        Assertion.createAnnotationPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), "en", false));
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(1, axioms.size());
        final Axiom<?> ax = axioms.iterator().next();
        final LangString value = (LangString) ax.getValue().getValue();
        assertEquals("a", value.getValue());
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
    void loadAxiomsLoadsStringLiteralWithCorrectLanguageForUnspecifiedPropertyType() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        persistLanguageTaggedStrings(individual);

        connector.begin();
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
        // Language is en
        descriptor.addAssertion(Assertion.createPropertyAssertion(URI.create(RDFS.LABEL.stringValue()), "en", false));
        final Collection<Axiom<?>> axioms = axiomLoader.loadAxioms(descriptor);
        assertEquals(1, axioms.size());
        final Axiom<?> ax = axioms.iterator().next();
        final LangString value = (LangString) ax.getValue().getValue();
        assertEquals("a", value.getValue());
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInAssertion() throws Exception {
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
        final LangString value = (LangString) ax.getValue().getValue();
        assertEquals("b", value.getValue());
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInAssertionOfUnspecifiedProperty() throws
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
        final LangString value = (LangString) ax.getValue().getValue();
        assertEquals("b", value.getValue());
    }

    @Test
    void loadsStringLiteralWithAllLanguagesWhenLanguageTagIsExplicitlySetToNull() throws Exception {
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
        assertTrue(values.contains(new LangString("a", "en").toString()));
        assertTrue(values.contains(new LangString("b", "cs").toString()));
    }

    @Test
    void loadAxiomsUsesSingleCallWhenLoadAllThresholdIsSetToLessThanAssertionCount() throws Exception {
        final DriverConfiguration driverConfig = new DriverConfiguration(TestRepositoryProvider.storageProperties());
        driverConfig.setProperty(Rdf4jConfigParam.LOAD_ALL_THRESHOLD, Integer.toString(1));
        final Connector spiedConnector = spy(connector);

        this.axiomLoader = new AxiomLoader(spiedConnector, new RuntimeConfiguration(driverConfig));
        spiedConnector.begin();
        try {
            final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
            final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(individual));
            final Iterator<Assertion> it = generatedData.values.get(individual).keySet().iterator();
            descriptor.addAssertion(it.next());
            descriptor.addAssertion(it.next());
            axiomLoader.loadAxioms(descriptor);
            verify(spiedConnector).findStatements(vf.createIRI(individual), null, null, false);
        } finally {
            spiedConnector.close();
        }
    }

    @Test
    void loadAxiomsLoadsAxiomsFromAllSpecifiedContexts() throws Exception {
        final String individual = generatedData.individuals.get(Generator.randomIndex(generatedData.individuals));
        final Assertion property = generatedData.values.get(individual).keySet().iterator().next();
        final URI contextOne = Generator.generateUri();
        final Object value = saveValueIntoContext(individual, property, contextOne.toString());
        final URI contextTwo = Generator.generateUri();
        final Object valueTwo = saveValueIntoContext(individual, property, contextTwo.toString());

        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(individual));
        desc.addAssertion(property);
        desc.addAssertionContext(property, contextOne).addAssertionContext(property, contextTwo);
        connector.begin();
        final Collection<Axiom<?>> res = axiomLoader.loadAxioms(desc);
        assertEquals(2, res.size());
        assertTrue(res.stream().anyMatch(a -> a.getValue().getValue().equals(value)));
        assertTrue(res.stream().anyMatch(a -> a.getValue().getValue().equals(valueTwo)));
    }
}
