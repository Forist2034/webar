use std::process::ExitCode;

fn main() -> ExitCode {
    webar_image_fetcher::main::<webar_stackexchange_core::image::source::ArchiveImage>(
        webar_image_fetcher::Config {
            https_only: true,
            user_agent: "curl",
        },
    )
}
