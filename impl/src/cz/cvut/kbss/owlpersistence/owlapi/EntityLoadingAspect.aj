package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.model.annotations.OWLClass;

public aspect EntityLoadingAspect {

	before() : staticinitialization(@OWLClass *) {
		MetamodelImpl.addClass(thisJoinPointStaticPart.getSignature()
				.getDeclaringType());
	}
}
