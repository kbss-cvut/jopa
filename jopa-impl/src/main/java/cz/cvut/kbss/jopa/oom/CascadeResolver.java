package cz.cvut.kbss.jopa.oom;

import java.net.URI;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

abstract class CascadeResolver {
	
	protected ObjectOntologyMapperImpl mapper;

	protected CascadeResolver(ObjectOntologyMapperImpl mapper) {
		this.mapper = mapper;
	}

	protected abstract void resolveFieldCascading(FieldSpecification<?, ?> fieldSpec, Object fieldValue, URI context);
}
