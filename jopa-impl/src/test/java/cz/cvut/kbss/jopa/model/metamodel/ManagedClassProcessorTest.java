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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.PostLoad;
import cz.cvut.kbss.jopa.model.annotations.PostPersist;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("unused")
public class ManagedClassProcessorTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void processManagedTypeThrowsInitializationExceptionWhenClassIsMissingNoArgConstructor() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage(
                "Class " + ClassWithoutNoArgConstructor.class + " is missing required no-arg constructor.");
        ManagedClassProcessor.processManagedType(ClassWithoutNoArgConstructor.class);
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithoutNoArgConstructor")
    private static class ClassWithoutNoArgConstructor {
        private String name;

        public ClassWithoutNoArgConstructor(String arg) {
            this.name = arg;
        }
    }

    @Test
    public void processManagedTypeReturnsEntityTypeForEntity() {
        final AbstractIdentifiableType<OWLClassA> res = ManagedClassProcessor.processManagedType(OWLClassA.class);
        assertTrue(res instanceof EntityType);
    }

    @Test
    public void processManagedTypeReturnsMappedSuperclassTypeForMappedSuperclass() {
        final AbstractIdentifiableType<QMappedSuperclass> res = ManagedClassProcessor
                .processManagedType(QMappedSuperclass.class);
        assertTrue(res instanceof MappedSuperclassType);
    }

    @Test
    public void processManagedTypeDiscoversEntityLifecycleListeners() throws Exception {
        final AbstractIdentifiableType<OWLClassR> type = ManagedClassProcessor.processManagedType(OWLClassR.class);
        assertTrue(type.hasLifecycleListeners(LifecycleEvent.POST_LOAD));
        assertEquals(1, type.getLifecycleListeners(LifecycleEvent.POST_LOAD).size());
        assertEquals(OWLClassR.class.getDeclaredMethod("postLoad"),
                type.getLifecycleListeners(LifecycleEvent.POST_LOAD).get(0));
    }

    @Test
    public void processManagedTypeThrowsMetamodelInitializationExceptionWhenMultipleListenersForOnePhaseAreDeclaredOnType()
            throws Exception {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("Type " + ClassWithMultipleListeners.class.getName() +
                " has multiple lifecycle callback methods for the same lifecycle event " + PostLoad.class.getName());
        ManagedClassProcessor.processManagedType(ClassWithMultipleListeners.class);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithMultipleListeners")
    private static class ClassWithMultipleListeners {
        @PostLoad
        private void one() {
        }

        @PostLoad
        private void two() {
        }
    }

    @Test
    public void processManagedTypeThrowsMetamodelInitializationExceptionWhenLifecycleListenerTakesArguments() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [takesArguments] in type [" +
                ClassWithLifecycleListenerTakingArguments.class.getName() +
                "] has an incorrect signature. It should not have any arguments.");
        ManagedClassProcessor.processManagedType(ClassWithLifecycleListenerTakingArguments.class);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithLifecycleListenerTakingArguments")
    private static class ClassWithLifecycleListenerTakingArguments {

        @PrePersist
        private void takesArguments(EntityManager em) {
        }
    }

    @Test
    public void processManagedTypeThrowsMetamodelInitializationExceptionWhenLifecycleIsNotVoid() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [returnsData] in type [" +
                ClassWithNonVoidLifecycleListener.class.getName() +
                "] has an incorrect signature. Its return type should be void.");
        ManagedClassProcessor.processManagedType(ClassWithNonVoidLifecycleListener.class);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithNonVoidLifecycleListener")
    private static class ClassWithNonVoidLifecycleListener {

        @PostPersist
        private String returnsData() {
            return "test";
        }
    }

    @Test
    public void processManagedTypeThrowsMetamodelInitializationExceptionWhenLifecycleListenerIsFinal() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [finalListener] in type [" +
                ClassWithFinalLifecycleListener.class.getName() +
                "] has an incorrect signature. It should not be static or final.");
        ManagedClassProcessor.processManagedType(ClassWithFinalLifecycleListener.class);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithFinalLifecycleListener")
    private static class ClassWithFinalLifecycleListener {

        @PostPersist
        public final void finalListener() {
        }
    }

    @Test
    public void processManagedTypeThrowsMetamodelInitializationExceptionWhenLifecycleListenerIsStatic() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [staticListener] in type [" +
                ClassWithStaticLifecycleListener.class.getName() +
                "] has an incorrect signature. It should not be static or final.");
        ManagedClassProcessor.processManagedType(ClassWithStaticLifecycleListener.class);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithStaticLifecycleListener")
    private static class ClassWithStaticLifecycleListener {

        @PostPersist
        public static void staticListener() {
        }
    }
}
