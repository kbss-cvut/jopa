/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public abstract class MultipleInheritanceTestRunner extends BaseRunner {
    protected OWLClassWithUnProperties classWithUnProperties;

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

        em.persist(child);
        em.clear();
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
        em.persist(classWithUnProperties);
        em.clear();

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
        em.persist(child);
        em.clear();
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

        em.persist(child);
        em.clear();
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

        em.getTransaction().begin();

        em.persist(childOfMappedSuperClass);

        em.getTransaction().commit();


        verifyExists(ChildOfMappedSuperClass.class, uri);
        em.clear();
        final ChildOfMappedSuperClass found = findRequired(ChildOfMappedSuperClass.class, uri);
        assertEquals(label, found.getLabel());
    }
}
