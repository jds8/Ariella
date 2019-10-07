module TypesModule

export Variable_Type, Dynamic_Type, Concrete_Type, Primitive_Type, Numeric_Type
export Float_Type, Int_Type, Bool_Type, Function_Type

# Define types for variables
abstract type Variable_Type end;
struct Dynamic_Type <: Variable_Type end;

# Define concrete types of variables
abstract type Concrete_Type <: Variable_Type end;
abstract type Primitive_Type <: Concrete_Type end;
abstract type Numeric_Type <: Primitive_Type end;
struct Float_Type <: Numeric_Type end;
struct Int_Type <: Numeric_Type end;
struct Bool_Type <: Primitive_Type end;

# Define function types for functions
struct Function_Type <: Concrete_Type
    arg_types::Array{Variable_Type,1};
    return_type::Variable_Type;
end

# Accessors for Function_Type
get_arg_types(ft::Function_Type) = ft.arg_types;
get_return_type(ft::Function_Type) = ft.return_type;

# Define operations on Primitive_Types
##################################################
import Base.+, Base.-, Base.*, Base./
+(x::Float_Type, y::Numeric_Type) = Float_Type();
-(x::Float_Type, y::Numeric_Type) = Float_Type();
*(x::Float_Type, y::Numeric_Type) = Float_Type();
/(x::Float_Type, y::Numeric_Type) = Float_Type();
+(x::Numeric_Type, y::Float_Type) = Float_Type();
-(x::Numeric_Type, y::Float_Type) = Float_Type();
*(x::Numeric_Type, y::Float_Type) = Float_Type();
/(x::Numeric_Type, y::Float_Type) = Float_Type();
+(x::Int_Type, y::Int_Type) = Int_Type();
-(x::Int_Type, y::Int_Type) = Int_Type();
*(x::Int_Type, y::Int_Type) = Int_Type();
/(x::Int_Type, y::Int_Type) = Float_Type();
# Note that dividing Ints results in a Float
##################################################

end
