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

import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

import java.util.List;
import java.util.Objects;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.getNPXMessageSupplier;

class SesameLists implements Lists {

    private final SesameAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    SesameLists(SesameAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public List<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor, "descriptor");
        return adapter.getSimpleListHandler().loadList(descriptor);
    }

    private void verifyArgs(ListDescriptor descriptor, String argName) throws SesameDriverException {
        beforeCallback.execute();
        Objects.requireNonNull(descriptor, getNPXMessageSupplier(argName));
    }

    @Override
    public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        verifyArgs(descriptor, "descriptor");
        adapter.getSimpleListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        verifyArgs(descriptor, "descriptor");
        adapter.getSimpleListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public List<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor, "descriptor");
        return adapter.getReferencedListHandler().loadList(descriptor);
    }

    @Override
    public void persistReferencedList(ReferencedListValueDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor, "descriptor");
        adapter.getReferencedListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateReferencedList(ReferencedListValueDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor, "descriptor");
        adapter.getReferencedListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }
}
