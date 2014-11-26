package cz.cvut.kbss.ontodriver.sesame;

import static cz.cvut.kbss.jopa.utils.ErrorUtils.constructNPXMessage;

import java.util.Collection;
import java.util.Objects;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Lists;
import cz.cvut.kbss.ontodriver_new.descriptors.ListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SesameLists implements Lists {

	private final SesameConnection connection;
	private final SesameAdapter adapter;

	public SesameLists(SesameConnection connection, SesameAdapter adapter) {
		this.connection = connection;
		this.adapter = adapter;
	}

	@Override
	public Collection<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		return adapter.getSimpleListHandler().loadList(descriptor);
	}

	private void verifyArgs(ListDescriptor descriptor, String argName) {
		connection.ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage(argName));
	}

	@Override
	public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getSimpleListHandler().persistList(descriptor);
		connection.commitIfAuto();
	}

	@Override
	public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getSimpleListHandler().updateList(descriptor);
		connection.commitIfAuto();
	}

	@Override
	public Collection<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		return adapter.getReferencedListHandler().loadList(descriptor);
	}

	@Override
	public void persistReferencedList(ReferencedListValueDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getReferencedListHandler().persistList(descriptor);
		connection.commitIfAuto();
	}

	@Override
	public void updateReferencedList(ReferencedListValueDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getReferencedListHandler().updateList(descriptor);
		connection.commitIfAuto();
	}
}
