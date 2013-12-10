package refl {

    // symbolic representation of a field.
    public interface RepField {
        function get name() : String;
        function get tp() : String;

        function getValue(self : Object) : void;
        function setValue(self : Object, value : Object) : void;
    }
}
