<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1412753508303" ID="ID_810440979" MODIFIED="1415186004299" TEXT="JOPA">
<node CREATED="1412753692348" ID="ID_296271803" MODIFIED="1412753697691" POSITION="right" TEXT="SesameDriver">
<node CREATED="1412753698987" ID="ID_553770748" MODIFIED="1412753700923" TEXT="Update">
<node CREATED="1412759214286" ID="ID_1466838461" MODIFIED="1412764189216" TEXT="Properties and currently inferred, allow modification in the future">
<node CREATED="1412764011312" ID="ID_687983346" MODIFIED="1412764049887" TEXT="In that case it will be necessary to modify update strategy in OntoDriver, using remover would remove too much"/>
</node>
<node CREATED="1412766318176" ID="ID_1887107087" MODIFIED="1412767224513" TEXT="Create strategy for working with types, which will merge types received from JOPA to those in the ontology">
<font NAME="SansSerif" SIZE="12"/>
<node CREATED="1412771026130" ID="ID_1952275794" MODIFIED="1412771041556" TEXT="In that case we need to be able to preserve the entity class assertion"/>
</node>
</node>
<node CREATED="1412760921710" ID="ID_1905414150" MODIFIED="1415181415532" TEXT="Design interface for lists in OntoDriver">
<icon BUILTIN="button_ok"/>
</node>
<node CREATED="1412764150384" ID="ID_1232791949" MODIFIED="1412764167810" TEXT="Add addTypes/removeTypes to OntoDriver API"/>
</node>
<node CREATED="1412758973998" ID="ID_616916095" MODIFIED="1412758991245" POSITION="left" TEXT="Types and properties should be handled separately in OntoDriver"/>
<node CREATED="1413380466924" ID="ID_714031943" MODIFIED="1413380489113" POSITION="right" TEXT="What happens when someone uses the same property for multiple fields in the entity"/>
<node CREATED="1413920392502" ID="ID_1382888775" MODIFIED="1413920449749" POSITION="left" TEXT="Lists">
<node CREATED="1413920406619" ID="ID_239344121" MODIFIED="1413920479858" TEXT="Should we add removeSimple(Ref)List, persistSimple(Ref)List etc?">
<node CREATED="1413920455161" ID="ID_1430539617" MODIFIED="1413920470596" TEXT="This would mean on update of a single element removal and persist of the whole list"/>
</node>
</node>
<node CREATED="1415186005171" ID="ID_1191722711" MODIFIED="1415186016737" POSITION="right" TEXT="Load all lazily loaded fields on entity detach"/>
</node>
</map>
