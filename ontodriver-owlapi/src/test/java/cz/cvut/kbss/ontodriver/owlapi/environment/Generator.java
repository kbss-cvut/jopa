/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.environment;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.UUID;

public class Generator {

    private static final Random RANDOM = new Random();

    private static final String URI_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/ontodriver/";

    private static final OWLDataFactory DATA_FACTORY = new OWLDataFactoryImpl();

    private Generator() {
        throw new AssertionError();
    }

    /**
     * Generates a (pseudo) random URI
     *
     * @return Random URI
     */
    public static URI generateUri() {
        return URI.create(URI_BASE + RANDOM.nextInt());
    }

    public static int randomInt(int max) {
        return RANDOM.nextInt(max);
    }

    /**
     * Generates a set of random OWLAxioms.
     * <p>
     * The size of the set is 50.
     *
     * @return Set of OWL axioms
     */
    public static Set<OWLAxiom> generateAxioms() {
        final Set<OWLAxiom> result = new HashSet<>(64);
        final int tBoxItemCount = 10;
        final int totalCount = 50;
        final List<OWLClass> classes = generateOwlClasses(tBoxItemCount);
        final List<OWLObjectProperty> ops = generateOwlObjectProperties(tBoxItemCount);
        final List<OWLDataProperty> dps = generateOwlDataProperties(tBoxItemCount);
        result.addAll(
                classes.stream().map(DATA_FACTORY::getOWLDeclarationAxiom).toList());
        result.addAll(ops.stream().map(DATA_FACTORY::getOWLDeclarationAxiom).toList());
        result.addAll(dps.stream().map(DATA_FACTORY::getOWLDeclarationAxiom).toList());
        result.addAll(generateAssertionAxioms(classes, ops, dps, totalCount - 3 * tBoxItemCount));
        return result;
    }

    private static List<OWLClass> generateOwlClasses(int count) {
        final List<OWLClass> result = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            result.add(DATA_FACTORY.getOWLClass(IRI.create(generateUri("Class"))));
        }
        return result;
    }

    private static String generateUri(String prefix) {
        return URI_BASE + prefix + RANDOM.nextInt();
    }

    private static List<OWLObjectProperty> generateOwlObjectProperties(int count) {
        final List<OWLObjectProperty> result = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            result.add(DATA_FACTORY.getOWLObjectProperty(IRI.create(generateUri("ObjectProperty"))));
        }
        return result;
    }

    private static List<OWLDataProperty> generateOwlDataProperties(int count) {
        final List<OWLDataProperty> result = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            result.add(DATA_FACTORY.getOWLDataProperty(IRI.create(generateUri("DataProperty"))));
        }
        return result;
    }

    private static List<OWLAxiom> generateAssertionAxioms(List<OWLClass> classes, List<OWLObjectProperty> ops,
                                                          List<OWLDataProperty> dps, int count) {
        final List<OWLAxiom> result = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            final int type = randomInt(3);
            switch (type) {
                case 0:
                    result.add(generateClassAssertion(classes));
                    break;
                case 1:
                    result.add(generateObjectPropertyAssertion(ops));
                    break;
                case 2:
                    result.add(generateDataPropertyAssertion(dps));
            }
        }
        return result;
    }

    private static OWLClassAssertionAxiom generateClassAssertion(List<OWLClass> classes) {
        final OWLNamedIndividual individual = DATA_FACTORY.getOWLNamedIndividual(IRI.create(generateUri("Individual")));
        return DATA_FACTORY.getOWLClassAssertionAxiom(classes.get(randomInt(classes.size())), individual);
    }

    private static OWLObjectPropertyAssertionAxiom generateObjectPropertyAssertion(List<OWLObjectProperty> ops) {
        final OWLNamedIndividual source = DATA_FACTORY.getOWLNamedIndividual(IRI.create(generateUri("Individual")));
        final OWLNamedIndividual target = DATA_FACTORY.getOWLNamedIndividual(IRI.create(generateUri("Individual")));
        return DATA_FACTORY.getOWLObjectPropertyAssertionAxiom(ops.get(randomInt(ops.size())), source, target);
    }

    private static OWLDataPropertyAssertionAxiom generateDataPropertyAssertion(List<OWLDataProperty> dps) {
        final OWLNamedIndividual individual = DATA_FACTORY.getOWLNamedIndividual(IRI.create(generateUri("Individual")));
        final int type = randomInt(4);
        final OWLDataProperty dp = dps.get(randomInt(dps.size()));
        return switch (type) {
            case 0 -> DATA_FACTORY.getOWLDataPropertyAssertionAxiom(dp, individual, RANDOM.nextInt());
            case 1 -> DATA_FACTORY.getOWLDataPropertyAssertionAxiom(dp, individual, RANDOM.nextDouble());
            case 2 -> DATA_FACTORY.getOWLDataPropertyAssertionAxiom(dp, individual, RANDOM.nextBoolean());
            case 3 -> DATA_FACTORY.getOWLDataPropertyAssertionAxiom(dp, individual, UUID.randomUUID().toString());
            default -> throw new IllegalArgumentException();
        };
    }
}
