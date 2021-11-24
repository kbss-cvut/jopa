/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ManagedTypeValueMergerTest {

    @Mock
    private UnitOfWorkImpl uow;

    @Mock
    private MetamodelImpl metamodel;

    private Descriptor descriptor;

    private FieldSpecification<OWLClassD, OWLClassA> refASpec;

    private ManagedTypeValueMerger sut;

    @BeforeEach
    public void setUp() throws Exception {
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.isEntityType(any())).thenAnswer(invocation -> {
            final Class<?> arg = (Class<?>) invocation.getArguments()[0];
            return metamodel.entity(arg) != null;
        });

        this.refASpec = mocks.forOwlClassD().owlClassAAtt();
        this.descriptor = new EntityDescriptor();
        this.sut = new ManagedTypeValueMerger(uow);
    }

    @Test
    public void mergeValueLoadsInstanceFromRepository() {
        final OWLClassA orig = Generators.generateOwlClassAInstance();
        final OWLClassA merged = new OWLClassA(orig);
        final OWLClassD target = new OWLClassD(Generators.createIndividualIdentifier());
        when(uow.readObject(OWLClassA.class, merged.getUri(), descriptor)).thenReturn(orig);

        sut.mergeValue(target, new ChangeRecordImpl(refASpec, merged), descriptor);
        verify(uow).readObject(OWLClassA.class, merged.getUri(), descriptor);
        assertSame(orig, target.getOwlClassA());
    }

    @Test
    public void mergeValueSetsValueDirectlyWhenItIsNull() {
        final OWLClassD target = new OWLClassD(Generators.createIndividualIdentifier());
        target.setOwlClassA(Generators.generateOwlClassAInstance());
        sut.mergeValue(target, new ChangeRecordImpl(refASpec, null), descriptor);
        assertNull(target.getOwlClassA());
    }

    @Test
    public void mergeValueSetsTheMergedValueDirectlyWhenItRepresentsANewInstance() {
        final OWLClassD target = new OWLClassD(Generators.createIndividualIdentifier());
        target.setOwlClassA(Generators.generateOwlClassAInstance());
        final OWLClassA merged = Generators.generateOwlClassAInstance();

        sut.mergeValue(target, new ChangeRecordImpl(refASpec, merged), descriptor);
        assertSame(merged, target.getOwlClassA());
    }

    @Test
    public void mergeSetsTheMergedValueDirectlyWhenItHasNoIdentifier() {
        final OWLClassD target = new OWLClassD(Generators.createIndividualIdentifier());
        target.setOwlClassA(Generators.generateOwlClassAInstance());
        final OWLClassA merged = Generators.generateOwlClassAInstance();
        merged.setUri(null);
        when(uow.readObject(any(), isNull(), any())).thenThrow(new NullPointerException());

        sut.mergeValue(target, new ChangeRecordImpl(refASpec, merged), descriptor);
        assertSame(merged, target.getOwlClassA());
    }

    @Test
    void mergeReplacesNewValueInChangeRecordWhenItIsReadFromUoW() {
        final OWLClassA orig = Generators.generateOwlClassAInstance();
        final OWLClassA merged = Generators.generateOwlClassAInstance();
        final OWLClassA loaded = new OWLClassA(merged);
        final OWLClassD target = new OWLClassD(Generators.createIndividualIdentifier());
        target.setOwlClassA(orig);
        when(uow.readObject(OWLClassA.class, merged.getUri(), descriptor)).thenReturn(loaded);
        final ChangeRecord changeRecord = new ChangeRecordImpl(refASpec, merged);

        sut.mergeValue(target, changeRecord, descriptor);
        assertEquals(loaded, target.getOwlClassA());
        assertEquals(loaded, changeRecord.getNewValue());
    }
}
