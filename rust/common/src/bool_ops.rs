pub trait BoolOptionOps {
    fn then_some_unit(self) -> Option<()>;
    fn then_none(self) -> Option<()>;
}

impl BoolOptionOps for bool {
    fn then_some_unit(self) -> Option<()> {
        if self { Some(()) } else { None }
    }

    fn then_none(self) -> Option<()> {
        if self { None } else { Some(()) }
    }
}

pub trait BoolResultOps {
    #[expect(clippy::missing_errors_doc)]
    fn then_ok_unit<E, FE>(self, error: FE) -> Result<(), E>
    where
        FE: FnOnce() -> E;

    #[expect(clippy::missing_errors_doc)]
    fn then_err_unit<E, FE>(self, error: FE) -> Result<(), E>
    where
        FE: FnOnce() -> E;
}

impl BoolResultOps for bool {
    fn then_ok_unit<E, FE>(self, error: FE) -> Result<(), E>
    where
        FE: FnOnce() -> E,
    {
        if self { Ok(()) } else { Err(error()) }
    }

    fn then_err_unit<E, FE>(self, error: FE) -> Result<(), E>
    where
        FE: FnOnce() -> E,
    {
        if self { Err(error()) } else { Ok(()) }
    }
}
