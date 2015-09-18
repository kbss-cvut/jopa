package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.model.metamodel.*;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

/**
 * Provides metamodel mock objects for the test classes.
 */
public class MetamodelMocks {

    @Mock
    private EntityType<OWLClassA> etA;
    @Mock
    private Identifier idA;
    @Mock
    private SingularAttribute<OWLClassA, ?> aStringAtt;
    @Mock
    private TypesSpecification<OWLClassA, ?> aTypes;

    @Mock
    private EntityType<OWLClassB> etB;
    @Mock
    private Identifier idB;
    @Mock
    private SingularAttribute<OWLClassB, ?> bStringAtt;
    @Mock
    private PropertiesSpecification<OWLClassB, ?> bProperties;

    @Mock
    private EntityType<OWLClassC> etC;
    @Mock
    private Identifier idC;
    @Mock
    private ListAttribute<OWLClassC, ?> cReferencedList;
    @Mock
    private ListAttribute<OWLClassC, ?> cSimpleList;

    @Mock
    private EntityType<OWLClassD> etD;
    @Mock
    private Identifier idD;
    @Mock
    private SingularAttribute<OWLClassD, OWLClassA> dOwlClassAAtt;

    public MetamodelMocks() throws Exception {
        MockitoAnnotations.initMocks(this);
        TestEnvironmentUtils.initOWLClassAMocks(etA, aStringAtt, aTypes, idA);
        TestEnvironmentUtils.initOWLClassBMocks(etB, bStringAtt, bProperties, idB);
        TestEnvironmentUtils.initOWLClassCMocks(etC, cSimpleList, cReferencedList, idC);
        TestEnvironmentUtils.initOWLClassDMocks(etD, dOwlClassAAtt, idD);
    }

    public OWLClassAMetamodel forOwlClassA() {
        return new OWLClassAMetamodel();
    }

    public OWLClassBMetamodel forOwlClassB() {
        return new OWLClassBMetamodel();
    }

    public OWLClassCMetamodel forOwlClassC() {
        return new OWLClassCMetamodel();
    }

    public OWLClassDMetamodel forOwlClassD() {
        return new OWLClassDMetamodel();
    }

    private class OWLClassAMetamodel {
        public EntityType<OWLClassA> getEntityType() {
            return MetamodelMocks.this.etA;
        }

        public Identifier getIdentifier() {
            return MetamodelMocks.this.idA;
        }

        public SingularAttribute<OWLClassA, ?> getStringAttribute() {
            return MetamodelMocks.this.aStringAtt;
        }

        public TypesSpecification<OWLClassA, ?> getTypesSpec() {
            return MetamodelMocks.this.aTypes;
        }
    }

    private class OWLClassBMetamodel {
        public EntityType<OWLClassB> getEntityType() {
            return MetamodelMocks.this.etB;
        }

        public Identifier getIdentifier() {
            return MetamodelMocks.this.idB;
        }

        public SingularAttribute<OWLClassB, ?> getStringAttribute() {
            return MetamodelMocks.this.bStringAtt;
        }

        public PropertiesSpecification<OWLClassB, ?> getTypesSpec() {
            return MetamodelMocks.this.bProperties;
        }
    }

    private class OWLClassCMetamodel {
        public EntityType<OWLClassC> getEntityType() {
            return MetamodelMocks.this.etC;
        }

        public Identifier getIdentifier() {
            return MetamodelMocks.this.idC;
        }

        public ListAttribute<OWLClassC, ?> getReferencedListAtt() {
            return MetamodelMocks.this.cReferencedList;
        }

        public ListAttribute<OWLClassC, ?> getSimpleListAtt() {
            return MetamodelMocks.this.cSimpleList;
        }
    }

    private class OWLClassDMetamodel {
        public EntityType<OWLClassD> getEntityType() {
            return MetamodelMocks.this.etD;
        }

        public Identifier getIdentifier() {
            return MetamodelMocks.this.idD;
        }

        public SingularAttribute<OWLClassD, OWLClassA> getOwlClassAAtt() {
            return MetamodelMocks.this.dOwlClassAAtt;
        }
    }
}
