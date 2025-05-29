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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.test.ChildOfMappedSuperClass;
import cz.cvut.kbss.jopa.test.OWLChildClassA;
import cz.cvut.kbss.jopa.test.OWLChildClassB;
import cz.cvut.kbss.jopa.test.OWLChildClassC;
import cz.cvut.kbss.jopa.test.OWLClassWithUnProperties;
import cz.cvut.kbss.jopa.test.OWLInterfaceA;
import cz.cvut.kbss.jopa.test.OWLInterfaceAnMethods;
import cz.cvut.kbss.jopa.test.OWLInterfaceB;
import cz.cvut.kbss.jopa.test.OWLParentA;
import cz.cvut.kbss.jopa.test.OWLParentB;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public abstract class MultipleInheritanceTestRunner extends BaseRunner {

    protected final OWLClassWithUnProperties classWithUnProperties;

    public MultipleInheritanceTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
        classWithUnProperties = new OWLClassWithUnProperties(Generators.generateUri());
    }


    @Test
    void entityCanBeFoundByBothParentTypes() {
        this.em = getEntityManager("entityCanBeFoundByBothParentTypes", false);

        URI id = Generators.generateUri();
        final OWLChildClassA child = new OWLChildClassA();
        child.setId(id);
        child.setStringAttribute("AttRVal");
        child.setPluralAnnotationProperty(Collections.singleton("seeet"));
        persist(child);

        final OWLChildClassA found = findRequired(OWLChildClassA.class, id);
        em.clear();
        final OWLParentB parentBFound = findRequired(OWLParentB.class, id);
        em.clear();
        final OWLParentA parentAFound = findRequired(OWLParentA.class, id);

        assertEquals(child.getId(), found.getId());
        assertEquals(child.getStringAttribute(), parentBFound.getStringAttribute());
        assertEquals(child.getPluralAnnotationProperty(), parentAFound.getPluralAnnotationProperty());
    }

    @Test
    void annotatedMethodPassesDownAnnotationValuesFromSingleParent() {
        this.em = getEntityManager("annotatedMethodPassesDownAnnotationValues", false);

        classWithUnProperties.setName("NAME_VALUE");
        classWithUnProperties.setTitles(Collections.singleton("title"));
        persist(classWithUnProperties);

        OWLClassWithUnProperties found = em.find(OWLClassWithUnProperties.class, classWithUnProperties.getId());

        IRI namePropertyIRI = em.getMetamodel().entity(OWLClassWithUnProperties.class).getDeclaredAttribute("name").getIRI();

        assertNotNull(found);
        assertEquals(classWithUnProperties.getName(), found.getName());
        assertEquals(classWithUnProperties.getId(), found.getId());
        assertEquals(Vocabulary.p_m_unannotated_name, namePropertyIRI.toString());
        assertEquals(classWithUnProperties.getTitles(), found.getTitles());

    }

    @Test
    void annotatedMethodPassesDownAnnotationValuesFromMultipleParents() {
        this.em = getEntityManager("annotatedMethodPassesDownAnnotationValuesFromMultipleParents", false);

        URI id = Generators.generateUri();
        final OWLChildClassB child = new OWLChildClassB();
        child.setId(id);
        child.setAttributeA("Value");
        child.setAttributeB(Boolean.FALSE);
        persist(child);

        final OWLChildClassB found = findRequired(OWLChildClassB.class, id);
        em.clear();
        final OWLInterfaceA parentAFound = findRequired(OWLInterfaceA.class, id);
        em.clear();
        final OWLInterfaceB parentBFound = findRequired(OWLInterfaceB.class, id);

        assertEquals(child.getId(), found.getId());
        assertEquals(child.getAttributeA(), parentAFound.getAttributeA());
        assertEquals(child.getAttributeB(), parentBFound.getAttributeB());

        EntityType<OWLChildClassB> childEt = em.getMetamodel().entity(OWLChildClassB.class);

        assertEquals(Vocabulary.p_m_attributeA, childEt.getDeclaredAttribute("attributeA").getIRI().toString());
        assertEquals(Vocabulary.p_m_attributeB, childEt.getDeclaredAttribute("attributeB").getIRI().toString());
    }

    @Test
    void annotationInheritedThroughTwoWaysIsHandledProperly() {
        this.em = getEntityManager("annotationInheritedThroughTwoWaysIsHandledProperly", false);
        URI id = Generators.generateUri();

        final OWLChildClassC child = new OWLChildClassC();
        child.setId(id);
        child.setName("Name");
        child.setAttributeB(Generators.randomBoolean());
        persist(child);

        final OWLChildClassC found = findRequired(OWLChildClassC.class, id);
        em.clear();
        final OWLInterfaceAnMethods commonParentFound = findRequired(OWLInterfaceAnMethods.class, id);

        assertEquals(child.getId(), found.getId());
        assertEquals(child.getName(), found.getName());
        assertEquals(child.getAttributeB(), found.getAttributeB());

        assertEquals(child.getName(), commonParentFound.getName());
    }

    @Test
    void mappedSuperClassSupportsAnnotatedMethods() {
        this.em = getEntityManager("mappedSuperClassSupportsAnnotatedMethods", false);

        ChildOfMappedSuperClass childOfMappedSuperClass = new ChildOfMappedSuperClass();
        URI uri = Generators.generateUri();

        String label = "LABEL_VALUE";
        childOfMappedSuperClass.setUri(uri);
        childOfMappedSuperClass.setLabel(label);

        persist(childOfMappedSuperClass);


        verifyExists(ChildOfMappedSuperClass.class, uri);
        em.clear();
        final ChildOfMappedSuperClass found = findRequired(ChildOfMappedSuperClass.class, uri);
        assertEquals(label, found.getLabel());
    }
}
